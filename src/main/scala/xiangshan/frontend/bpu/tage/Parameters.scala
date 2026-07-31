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
import xiangshan.frontend.bpu.HasBpuParameters
import xiangshan.frontend.bpu.TageTableInfo

case class TageHistoryProfile(
    MinHistoryLength:    Int,
    MaxHistoryLength:    Int,
    TableHistoryLengths: Seq[Int]
)

case class TageHistoryDseParameters(
    MinHistoryLengths: Seq[Int],
    MaxHistoryLengths: Seq[Int]
)

case class TageParameters(
    TableInfos: Seq[TageTableInfo] = Seq(
      // (NumSetsLog2, NumWays, HistoryLength); NumSets is the number of sets in each bank.
      new TageTableInfo(9, 2, 4),
      new TageTableInfo(9, 2, 9),
      new TageTableInfo(9, 2, 17),
      new TageTableInfo(9, 2, 29),
      new TageTableInfo(9, 2, 56),
      new TageTableInfo(9, 2, 109),
      new TageTableInfo(9, 2, 211),
      new TageTableInfo(9, 2, 397)
    ),
    NumBanks:              Int = 4,   // to alleviate read-write conflicts in single-port SRAM
    TagWidth:              Int = 13,  // default active tag width
    TakenCtrWidth:         Int = 3,
    UsefulCtrWidth:        Int = 2,   // default active useful counter width
    UsefulCtrInitValue:    Int = 0,
    NumUsefulCtrSramFolds: Int = 8,
    WriteBufferSize:       Int = 4,
    UsefulResetCtrWidth:   Int = 8,   // default active useful-reset counter width
    UseAltOnNaWidth:       Int = 7,   // default active use-alt counter width
    NumUseAltOnNa:         Int = 128, // default active number of use-alt counters
    EnableTageTrace:       Boolean = false,
    // constantin
    MinNumSetsLog2:         Int = 7,
    MaxNumSetsLog2:         Int = 12,
    MinNumWays:             Int = 2,
    MaxNumWays:             Int = 8,
    MinTagWidth:            Int = 5,
    MaxTagWidth:            Int = 15,
    MinUsefulCtrWidth:      Int = 1,
    MaxUsefulCtrWidth:      Int = 2,
    MinUsefulResetCtrWidth: Int = 4,
    MaxUsefulResetCtrWidth: Int = 10,
    MinUseAltOnNaWidth:     Int = 2,
    MaxUseAltOnNaWidth:     Int = 8,
    MinNumUseAltOnNa:       Int = 16,
    MaxNumUseAltOnNa:       Int = 512,
    HistoryDse:             Option[TageHistoryDseParameters] = None
) {
  val SupportedMinHistoryLengths: Seq[Int] =
    HistoryDse.map(_.MinHistoryLengths).getOrElse(Seq(TableInfos.head.HistoryLength))
  val SupportedMaxHistoryLengths: Seq[Int] =
    HistoryDse.map(_.MaxHistoryLengths).getOrElse(Seq(TableInfos.last.HistoryLength))

  def getGeometricHistoryLengths(minHistoryLength: Int, maxHistoryLength: Int): Seq[Int] = {
    require(minHistoryLength > 0, "TAGE minimum history length must be positive")
    require(maxHistoryLength > minHistoryLength, "TAGE maximum history length must be greater than the minimum")
    val alpha = math.pow(maxHistoryLength.toDouble / minHistoryLength, 1.0 / (TableInfos.length - 1))
    (0 until TableInfos.length).map(tableIdx => math.floor(minHistoryLength * math.pow(alpha, tableIdx) + 0.5).toInt)
  }

  require(TableInfos.length >= 2, "TAGE must have at least two tagged tables")
  require(SupportedMinHistoryLengths.nonEmpty, "TAGE minimum history length candidates must not be empty")
  require(SupportedMaxHistoryLengths.nonEmpty, "TAGE maximum history length candidates must not be empty")
  require(SupportedMinHistoryLengths.distinct.length == SupportedMinHistoryLengths.length)
  require(SupportedMaxHistoryLengths.distinct.length == SupportedMaxHistoryLengths.length)
  require(SupportedMinHistoryLengths.forall(_ > 0), "TAGE minimum history lengths must be positive")
  require(SupportedMaxHistoryLengths.forall(_ > 0), "TAGE maximum history lengths must be positive")
  require(
    SupportedMinHistoryLengths.forall(min => SupportedMaxHistoryLengths.forall(_ > min)),
    "Each TAGE maximum history length must be greater than every supported minimum"
  )
  require(SupportedMinHistoryLengths.contains(TableInfos.head.HistoryLength))
  require(SupportedMaxHistoryLengths.contains(TableInfos.last.HistoryLength))

  val DefaultHistoryProfile: TageHistoryProfile = TageHistoryProfile(
    TableInfos.head.HistoryLength,
    TableInfos.last.HistoryLength,
    TableInfos.map(_.HistoryLength)
  )

  val GeometricHistoryProfiles: Seq[TageHistoryProfile] =
    for {
      minHistoryLength <- SupportedMinHistoryLengths
      maxHistoryLength <- SupportedMaxHistoryLengths
    } yield TageHistoryProfile(
      minHistoryLength,
      maxHistoryLength,
      getGeometricHistoryLengths(minHistoryLength, maxHistoryLength)
    )

  val SupportedHistoryProfiles: Seq[TageHistoryProfile] =
    DefaultHistoryProfile +: GeometricHistoryProfiles.filterNot { profile =>
      profile.MinHistoryLength == DefaultHistoryProfile.MinHistoryLength &&
      profile.MaxHistoryLength == DefaultHistoryProfile.MaxHistoryLength
    }

  val MaxSupportedHistoryLength: Int = SupportedMaxHistoryLengths.max

  require(SupportedHistoryProfiles.forall(profile => profile.MaxHistoryLength > profile.MinHistoryLength))
  SupportedHistoryProfiles.foreach { profile =>
    require(
      profile.TableHistoryLengths.zip(profile.TableHistoryLengths.tail).forall { case (shorter, longer) =>
        shorter < longer
      },
      s"TAGE history profile (${profile.MinHistoryLength}, ${profile.MaxHistoryLength}) is not strictly increasing"
    )
  }
  require(TableInfos.forall(info => info.NumSetsLog2 >= MinNumSetsLog2 && info.NumSetsLog2 <= MaxNumSetsLog2))
  require(TableInfos.forall(info => info.NumWays >= MinNumWays && info.NumWays <= MaxNumWays))
  require(TagWidth >= MinTagWidth && TagWidth <= MaxTagWidth)
  require(UsefulCtrWidth >= MinUsefulCtrWidth && UsefulCtrWidth <= MaxUsefulCtrWidth)
  require(UsefulResetCtrWidth >= MinUsefulResetCtrWidth && UsefulResetCtrWidth <= MaxUsefulResetCtrWidth)
  require(UseAltOnNaWidth >= MinUseAltOnNaWidth && UseAltOnNaWidth <= MaxUseAltOnNaWidth)
  require(NumUseAltOnNa >= MinNumUseAltOnNa && NumUseAltOnNa <= MaxNumUseAltOnNa)
  require(MinNumSetsLog2 > 0 && MinNumSetsLog2 <= MaxNumSetsLog2)
  require(MinUsefulResetCtrWidth > 0 && MinUsefulResetCtrWidth <= MaxUsefulResetCtrWidth)
  require(MinUseAltOnNaWidth > 0 && MinUseAltOnNaWidth <= MaxUseAltOnNaWidth)
  require(MinNumUseAltOnNa > 0 && MinNumUseAltOnNa <= MaxNumUseAltOnNa)
  require(isPow2(MinNumUseAltOnNa) && isPow2(MaxNumUseAltOnNa) && isPow2(NumUseAltOnNa))
}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def NumBanks:              Int = tageParameters.NumBanks
  def BankIdxWidth:          Int = log2Ceil(NumBanks)
  def TagWidth:              Int = tageParameters.MaxTagWidth
  def DefaultTagWidth:       Int = tageParameters.TagWidth
  def TakenCtrWidth:         Int = tageParameters.TakenCtrWidth
  def UsefulCtrWidth:        Int = tageParameters.MaxUsefulCtrWidth
  def DefaultUsefulCtrWidth: Int = tageParameters.UsefulCtrWidth
  def UsefulCtrInitValue:    Int = tageParameters.UsefulCtrInitValue
  def WriteBufferSize:       Int = tageParameters.WriteBufferSize

  def UsefulResetCtrWidth:        Int = tageParameters.MaxUsefulResetCtrWidth
  def DefaultUsefulResetCtrWidth: Int = tageParameters.UsefulResetCtrWidth
  def UseAltOnNaWidth:            Int = tageParameters.MaxUseAltOnNaWidth
  def DefaultUseAltOnNaWidth:     Int = tageParameters.UseAltOnNaWidth
  def NumUseAltOnNa:              Int = tageParameters.MaxNumUseAltOnNa
  def DefaultNumUseAltOnNa:       Int = tageParameters.NumUseAltOnNa

  def TableInfos: Seq[TageTableInfo] = tageParameters.TableInfos

  def NumTables:     Int = TableInfos.length
  def TableIdxWidth: Int = log2Ceil(NumTables)

  def MinNumSetsLog2: Int = tageParameters.MinNumSetsLog2
  def MaxNumSetsLog2: Int = tageParameters.MaxNumSetsLog2
  def MaxSetIdxWidth: Int = MaxNumSetsLog2

  def MinNumWays:     Int = tageParameters.MinNumWays
  def MaxNumWays:     Int = tageParameters.MaxNumWays
  def MaxWayIdxWidth: Int = log2Ceil(MaxNumWays)

  def ActiveNumSetsLog2Width:         Int = log2Ceil(MaxNumSetsLog2 + 1)
  def ActiveNumWaysWidth:             Int = log2Ceil(MaxNumWays + 1)
  def ActiveTagWidthWidth:            Int = log2Ceil(TagWidth + 1)
  def ActiveUsefulCtrWidthWidth:      Int = log2Ceil(UsefulCtrWidth + 1)
  def ActiveUsefulResetCtrWidthWidth: Int = log2Ceil(UsefulResetCtrWidth + 1)
  def ActiveUseAltOnNaWidthWidth:     Int = log2Ceil(UseAltOnNaWidth + 1)
  def ActiveNumUseAltOnNaWidth:       Int = log2Ceil(NumUseAltOnNa + 1)
  def ActiveHistoryProfileWidth:      Int = log2Ceil(tageParameters.SupportedHistoryProfiles.length).max(1)

  def SupportedNumSetsLog2: Seq[Int] =
    MinNumSetsLog2 to MaxNumSetsLog2
  def SupportedTagWidths: Seq[Int] =
    tageParameters.MinTagWidth to tageParameters.MaxTagWidth
  def SupportedHistoryProfiles: Seq[TageHistoryProfile] =
    tageParameters.SupportedHistoryProfiles
  def MaxSupportedHistoryLength: Int =
    tageParameters.MaxSupportedHistoryLength

  // per table parameters
  def NumSets(implicit info:     TageTableInfo): Int = 1 << MaxNumSetsLog2
  def SetIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumSets)
  def NumWays(implicit info:     TageTableInfo): Int = MaxNumWays
  def WayIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumWays)

  def NumUsefulCtrSramFolds: Int = tageParameters.NumUsefulCtrSramFolds

  def EnableTageTrace: Boolean = tageParameters.EnableTageTrace
}
