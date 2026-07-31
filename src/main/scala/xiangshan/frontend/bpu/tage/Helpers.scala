// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import utils.AddrField
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterInit
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

trait TopHelper extends HasTageParameters {
  def getFoldedHist(
      allFoldedPathHist: PhrAllFoldedHistories,
      tableConfigs:      Seq[TageTableConfig],
      runtimeConfig:     TageRuntimeConfig
  ): Seq[TageFoldedHist] = {
    require(tableConfigs.length == NumTables)
    TableInfos.zip(tableConfigs).zipWithIndex.map { case ((tableInfo, tableConfig), tableIdx) =>
      val physicalTableInfo = new TageTableInfo(
        MaxNumSetsLog2,
        MaxNumWays,
        tableInfo.HistoryLength
      )

      val selectedHistoryLength = MuxCase(
        tableInfo.HistoryLength.U,
        SupportedHistoryProfiles.zipWithIndex.map { case (profile, profileIdx) =>
          (runtimeConfig.historyProfile === profileIdx.U) -> profile.TableHistoryLengths(tableIdx).U
        }
      )
      val supportedTableHistoryLengths =
        SupportedHistoryProfiles.map(_.TableHistoryLengths(tableIdx)).distinct

      def getFoldedHistory(historyLength: Int, foldedLength: Int): UInt =
        allFoldedPathHist
          .getHistWithInfo(new FoldedHistoryInfo(historyLength, math.min(historyLength, foldedLength)))
          .foldedHist

      val idxCandidates = for {
        historyLength <- supportedTableHistoryLengths
        numSetsLog2   <- SupportedNumSetsLog2
      } yield {
        val selected = selectedHistoryLength === historyLength.U && tableConfig.numSetsLog2 === numSetsLog2.U
        selected -> getFoldedHistory(historyLength, numSetsLog2)
      }

      val tagCandidates = for {
        historyLength <- supportedTableHistoryLengths
        tagWidth      <- SupportedTagWidths
      } yield {
        val selected  = selectedHistoryLength === historyLength.U && runtimeConfig.tagWidth === tagWidth.U
        val forTag    = getFoldedHistory(historyLength, tagWidth)
        val forAltTag = Cat(getFoldedHistory(historyLength, tagWidth - 1), 0.U(1.W))
        selected -> (forTag, forAltTag)
      }

      val foldedHist = Wire(new TageFoldedHist()(p, physicalTableInfo))
      foldedHist.forIdx := MuxCase(idxCandidates.head._2, idxCandidates)
      foldedHist.forTag(0) := MuxCase(
        tagCandidates.head._2._1,
        tagCandidates.map { case (sel, tags) => sel -> tags._1 }
      )
      foldedHist.forTag(1) := MuxCase(
        tagCandidates.head._2._2,
        tagCandidates.map { case (sel, tags) => sel -> tags._2 }
      )
      foldedHist
    }
  }

  def getLongestHistTableOH(hitTableMask: Seq[Bool]): Seq[Bool] =
    PriorityEncoderOH(hitTableMask.reverse).reverse

  def getActiveTagWidth(requestedWidth: UInt): UInt =
    Mux(
      requestedWidth < tageParameters.MinTagWidth.U,
      tageParameters.MinTagWidth.U,
      Mux(
        requestedWidth > tageParameters.MaxTagWidth.U,
        tageParameters.MaxTagWidth.U,
        requestedWidth
      )
    )

  def getActiveUsefulCtrWidth(requestedWidth: UInt): UInt =
    Mux(requestedWidth < 1.U, 1.U, Mux(requestedWidth > UsefulCtrWidth.U, UsefulCtrWidth.U, requestedWidth))

  def getActiveUsefulCtrValue(counter: SaturateCounter, requestedWidth: UInt): UInt = {
    val width = getActiveUsefulCtrWidth(requestedWidth)
    counter.value & ((1.U((UsefulCtrWidth + 1).W) << width) - 1.U)(UsefulCtrWidth - 1, 0)
  }

  def normalizeUsefulCtr(counter: SaturateCounter, requestedWidth: UInt): SaturateCounter =
    SaturateCounterInit(UsefulCtrWidth, getActiveUsefulCtrValue(counter, requestedWidth))

  def usefulCtrIsSaturateNegative(counter: SaturateCounter, requestedWidth: UInt): Bool =
    !getActiveUsefulCtrValue(counter, requestedWidth).orR

  def usefulCtrIsSaturatePositive(counter: SaturateCounter, requestedWidth: UInt): Bool = {
    val value = getActiveUsefulCtrValue(counter, requestedWidth)
    val width = getActiveUsefulCtrWidth(requestedWidth)
    value === ((1.U((UsefulCtrWidth + 1).W) << width) - 1.U)(UsefulCtrWidth - 1, 0)
  }

  def getUsefulCtrIncrease(counter: SaturateCounter, requestedWidth: UInt, en: Bool): SaturateCounter = {
    val value = getActiveUsefulCtrValue(counter, requestedWidth)
    SaturateCounterInit(
      UsefulCtrWidth,
      Mux(en && !usefulCtrIsSaturatePositive(counter, requestedWidth), value + 1.U, value)
    )
  }

  def getActiveUsefulResetCtrWidth(requestedWidth: UInt): UInt =
    Mux(
      requestedWidth < tageParameters.MinUsefulResetCtrWidth.U,
      tageParameters.MinUsefulResetCtrWidth.U,
      Mux(
        requestedWidth > tageParameters.MaxUsefulResetCtrWidth.U,
        tageParameters.MaxUsefulResetCtrWidth.U,
        requestedWidth
      )
    )

  def getActiveUsefulResetCtrValue(counter: SaturateCounter, requestedWidth: UInt): UInt = {
    val width = getActiveUsefulResetCtrWidth(requestedWidth)
    counter.value & ((1.U((UsefulResetCtrWidth + 1).W) << width) - 1.U)(UsefulResetCtrWidth - 1, 0)
  }

  def usefulResetCtrIsSaturatePositive(counter: SaturateCounter, requestedWidth: UInt): Bool = {
    val width    = getActiveUsefulResetCtrWidth(requestedWidth)
    val maxValue = ((1.U((UsefulResetCtrWidth + 1).W) << width) - 1.U)(UsefulResetCtrWidth - 1, 0)
    getActiveUsefulResetCtrValue(counter, requestedWidth) === maxValue
  }

  def getUsefulResetCtrIncrease(counter: SaturateCounter, requestedWidth: UInt): SaturateCounter = {
    val value = getActiveUsefulResetCtrValue(counter, requestedWidth)
    SaturateCounterInit(
      UsefulResetCtrWidth,
      Mux(usefulResetCtrIsSaturatePositive(counter, requestedWidth), value, value + 1.U)
    )
  }

  def getActiveUseAltOnNaWidth(requestedWidth: UInt): UInt =
    Mux(
      requestedWidth < tageParameters.MinUseAltOnNaWidth.U,
      tageParameters.MinUseAltOnNaWidth.U,
      Mux(
        requestedWidth > tageParameters.MaxUseAltOnNaWidth.U,
        tageParameters.MaxUseAltOnNaWidth.U,
        requestedWidth
      )
    )

  def getActiveUseAltOnNaValue(counter: SaturateCounter, requestedWidth: UInt): UInt = {
    val width = getActiveUseAltOnNaWidth(requestedWidth)
    counter.value & ((1.U((UseAltOnNaWidth + 1).W) << width) - 1.U)(UseAltOnNaWidth - 1, 0)
  }

  def useAltOnNaIsPositive(counter: SaturateCounter, requestedWidth: UInt): Bool = {
    val width    = getActiveUseAltOnNaWidth(requestedWidth)
    val signMask = (1.U((UseAltOnNaWidth + 1).W) << (width - 1.U))(UseAltOnNaWidth - 1, 0)
    (getActiveUseAltOnNaValue(counter, requestedWidth) & signMask).orR
  }

  def getUseAltOnNaUpdate(
      counter:        SaturateCounter,
      requestedWidth: UInt,
      increase:       Bool,
      decrease:       Bool
  ): SaturateCounter = {
    val width    = getActiveUseAltOnNaWidth(requestedWidth)
    val value    = getActiveUseAltOnNaValue(counter, requestedWidth)
    val maxValue = ((1.U((UseAltOnNaWidth + 1).W) << width) - 1.U)(UseAltOnNaWidth - 1, 0)
    SaturateCounterInit(
      UseAltOnNaWidth,
      Mux(
        increase && value =/= maxValue,
        value + 1.U,
        Mux(decrease && value.orR, value - 1.U, value)
      )
    )
  }

  def getActiveNumUseAltOnNa(requestedNum: UInt): UInt =
    Mux(
      requestedNum < tageParameters.MinNumUseAltOnNa.U,
      tageParameters.MinNumUseAltOnNa.U,
      Mux(
        requestedNum > tageParameters.MaxNumUseAltOnNa.U,
        tageParameters.MaxNumUseAltOnNa.U,
        requestedNum
      )
    )

  def getUseAltOnNaIdx(pc: PrunedAddr, requestedNum: UInt): UInt = {
    val useAltOnNaIdxWidth = log2Ceil(NumUseAltOnNa).max(1)
    val rawIdx             = pc(useAltOnNaIdxWidth - 1 + instOffsetBits, instOffsetBits)
    val activeMask         = (getActiveNumUseAltOnNa(requestedNum) - 1.U)(useAltOnNaIdxWidth - 1, 0)
    rawIdx & activeMask
  }
}

trait TableHelper extends TopHelper { // extends TopHelper for getBankIndex
  // varies between different tables
  implicit val info: TageTableInfo

  val addrFields = AddrField(
    Seq(
      ("instOffset", instOffsetBits),
      ("bankIdx", BankIdxWidth),
      ("setIdx", SetIdxWidth),
      ("tag", TagWidth)
    ),
    maxWidth = Option(VAddrBits)
  )

  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("bankIdx", pc)

  def getSetIndex(pc: PrunedAddr, hist: UInt): UInt =
    addrFields.extract("setIdx", pc) ^ hist

  def getPcTag(pc: PrunedAddr, requestedNumSetsLog2: UInt): UInt = {
    val activeSetIdxWidth = getActiveNumSetsLog2(requestedNumSetsLog2)
    (pc.toUInt >> (instOffsetBits.U + BankIdxWidth.U + activeSetIdxWidth))(TagWidth - 1, 0)
  }

  def getActiveNumSetsLog2(requestedNumSetsLog2: UInt): UInt =
    Mux(
      requestedNumSetsLog2 < MinNumSetsLog2.U,
      MinNumSetsLog2.U,
      Mux(requestedNumSetsLog2 > MaxNumSetsLog2.U, MaxNumSetsLog2.U, requestedNumSetsLog2)
    )

  def getActiveNumSets(requestedNumSetsLog2: UInt): UInt =
    UIntToOH(getActiveNumSetsLog2(requestedNumSetsLog2), SetIdxWidth + 1)

  def maskSetIndex(setIdx: UInt, requestedNumSetsLog2: UInt): UInt =
    setIdx & (getActiveNumSets(requestedNumSetsLog2) - 1.U)(SetIdxWidth - 1, 0)

  def getActiveNumWays(requestedNumWays: UInt): UInt =
    Mux(requestedNumWays < MinNumWays.U, MinNumWays.U, Mux(requestedNumWays > NumWays.U, NumWays.U, requestedNumWays))

  def getActiveWayMask(requestedNumWays: UInt): UInt =
    ((1.U((NumWays + 1).W) << getActiveNumWays(requestedNumWays)) - 1.U)(NumWays - 1, 0)

  def getActiveTagMask(requestedTagWidth: UInt): UInt =
    ((1.U((TagWidth + 1).W) << getActiveTagWidth(requestedTagWidth)) - 1.U)(TagWidth - 1, 0)

  def getRawTag(pc: PrunedAddr, hist: Vec[UInt], requestedNumSetsLog2: UInt): UInt =
    getPcTag(pc, requestedNumSetsLog2) ^ hist(0) ^ hist(1)

  def getTag(pc: PrunedAddr, hist: Vec[UInt], position: UInt, requestedNumSetsLog2: UInt): UInt =
    getRawTag(pc, hist, requestedNumSetsLog2) ^ position
}
