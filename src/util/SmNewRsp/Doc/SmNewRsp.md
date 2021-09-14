# `SmNewRsp` StateMod Utility Program

StateMod New Response File Program

The purpose of the program SmNewRsp is to read an old (version 1-10) StateMod response file and build a response file in the currently supported format (Version 11 and greater).

* [Disclaimer](#disclaimer)
* [Acknowledgment](#acknowledgment)
* [Introduction](#introduction)
* [Program Description](#program-description)
* [Input File Description](#input-file-description)
* [Output File Description](#output-file-description)
* [Comments and Concerns](#comments-and-concerns)

---------------


## Disclaimer

This program is furnished by The State of Colorado (State) and is accepted and used by the recipient upon the expressed understanding that the State makes no warranties,
express or implied, concerning the accuracy, completeness, reliability, usability,
or suitability for any particular purpose of the information and data contained in this program or furnished in connection therewith,
and the State shall be under no liability whatsoever to any person by reason of any use made thereof. 

The program herein belongs to the State of Colorado.
Therefore, the recipient further agrees not to assert any proprietary rights therein or
to further represent this program to anyone as other than a State program.

## Acknowledgment

This program was developed by Ray Bennett of the Division of Water Resources as part of Colorado’s Decision Support Systems.

## Introduction

The program `SmNewRsp` reads an old (version 1-10) StateMod response file and creates a new one in the currently supported format (version 11 and greater).

## Program Description

`SmNewRsp` is a preprocessing program that converts outdated (version 1-10) StateMod response file to a new (version 11 and greater) StateMod response file.
The new format allows files to be provided in any order using a file descriptor.
As presented below, `SmNewRsp` keys on the suffix recommended and typically used in an existing StateMod response file to
build a response file that is consistent with version 11 and greater.
If any existing file that does not contain a standard,
recommended suffix `SmNewRsp` will warn the user but will not try to determine the file type.
In such a case the results of `SmNewRsp` typically require hand editing before they can be successfully used by StateMod.
To execute `SmNewRsp`:

```
SmNewRsp flname.rsp 
```

where `flname.rsp` is an old (version 1-10) StateMod response.
The new response file is named `SmNewRsp.out`.
Messages are written to the log file named `SmNewRsp.log`.

| **#**	| **Standard File Extension** | **Descriptor** | **Example** |
| -- | -- | -- | -- |
|   1 | `*.ctl` | `Control` | `rgTWD.ctl` |
|   2 | `*.rin` | `River_Network` | `rgTW.rin` |
|   3 | `*.res` | `Reservoir_Station` | `rgTW.res` |
|   4 | `*.dds` | `Diversion_Station ` | `rgTW.dds` |
|   5 | `*.ris` | `StreamGage_Station ` | `rgTW.ris` |
|   6 | `*.ifs` | `Instreamflow_Station` | `rgTW.ifs` |
|   7 | `*.wes` | `Well_Station` | `rgTW.wes` |
|   8 | `*.ifr` | `Instreamflow_Right` | `rgTW.ifr` |
|   9 | `*.rer` | `Reservoir_Right` | `rgTW.rer` |
|  10 | `*.ddr` | `Diversion_Right` | `rgTW.ddr` |
|  11 | `*.opr` | `Operational_Right` | `rgTW.opr` |
|  12 | `*.wer` | `Well_Right` | `rgTW.wer` |
|  13 | `*.dum` | `Precipitation Monthly` | `rgTW.pre` |
|  14 | `*.eva` | `Evaporation_Monthly` | `rgTW.eva` |
|  15 | `*.rim` | `Stream_Base_Monthly` | `rgtw.rim` |
|  16 | `*.ddm` | `Diversion_Demand_Monthly` | `rgTW.ddm` |
|  17 | `*.dda` | `Diversion Demand Average Monthly` | `rgTW.dda` |
|  18 | `*.ddo` | `Diversion Demand Override` | `rgTW.ddo` |
|  19 | `*.ifm` | `Instreamflow_Demand_Monthly` | `rgTW.ifm` |
|  20 | `*.ifa` | `Instreamflow_Demand_AverageMonthly` | `rgTW.ifa` |
|  21 | `*.wem` | `Well_Demand_Monthly` | `rgTW.wem` |
|  22 | `*.dly` | `DelayTable_Monthly ` | `rgTW.dly` |
|  23 | `*.tar` | `Reservoir_Target_Monthly` | `rgTW.tar` |
|  24 | `*.tsp` | `IrrigationPractice_Yearly` | `rg.tsp` |
|  25 | `*.iwr` | `ConsumptiveWaterRequirement_Monthly` | `rg.iwr` |
|  26 | `*.par` | `SoilMoisture` | `rg.par` |
|  27 | `*.eom` | `Reservoir_Historic_Monthly` | `rgTW.eom` |
|  28 | `*.rib` | `StreamEstimate_Coefficients` | `rgTW.rib` |
|  29 | `*.rih` | `StreamGage_Historic_Monthly` | `rgTW.rih` |
|  30 | `*.ddh` | `Diversion_Historic_Monthly` | `rgTW.ddh` |
|  31 | `*.weh` | `Well_Historic_Monthly` | `rgTW.weh` |
|  32 | `*.gvp` | `GeographicInformation` | `rgTW_StateMod.gvp` |
|  33 | `*.out` | `OutputRequest` | `RgTW.out` |
|  34 | `*.rid` | `Stream_Base_Daily` | `rgTWD.rid` |
|  35 | `*.dum` | `Dummy` | `rgTWD.dum` |
|  36 | `*.dum` | `Dummy` | `rgTWD.dum` |
|  37 | `*.dum` | `Dummy` | `rgTWD.dum` |
|  38 | `*.dum` | `Dummy` | `rgTWD.dum` |
|  39 | `*.dld` | `DelayTable_Daily` | `rgTwD.dld` |
|  40 | `*.iwd` | `ConsumptiveWaterRequirement_Daily` | `rgTWD.iwd` |
|  41 | `*.rhy` | `StreamGage_Historic_Daily` | `rgTWD.rhy` |
|  42 | `*.dhy` | `Diversion_Historic_Daily ` | `RgTWD.dhy` |
|  43 | `*.why` | `Well_Historic_Daily` | `RgTWD.why` |
|  44 | `*.eoy` | `Reservoir_Historic_Daily` | `RgTWD.eoy` |

## Input File Description

Program input is an old format respose file, specified as the command parameter.

## Output File Description

`SmNewRsp` creates a log file and a StateMod new (version 11 and greater) format.

## Comments and Concerns

Following are comments and concerns associated with the preprocessor `SmNewRsp`:

* The old StateMod response file required dummy files be provided if a particular data type is not used.
Accordingly, `SmNewRsp` ignores any files with a `*.dum` file extension.