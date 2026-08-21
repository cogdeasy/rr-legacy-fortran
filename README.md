# EHMS - ENGINE HEALTH MONITORING SYSTEM, RELEASE 4.2

Rolls-Royce plc - Civil Aero Support - Derby

EHMS is the overnight batch suite that turns raw engine downlink data into
the fleet status report used by the duty controller and the shop loading
clerk. It covers telemetry validation, deterioration trending, exceedance
alerting, remaining useful life projection and shop visit planning.

The suite is written in fixed form FORTRAN 77, punched in columns 1 to 72,
with shared data held in labelled `COMMON` and declared in include decks.
All files are formatted sequential data sets. There are no external
libraries.

## Job stream

| Step | Program | Function                                        |
|------|---------|-------------------------------------------------|
| 010  | EHMGEN  | Build the engine master and the downlink log     |
| 020  | EHMLOD  | Validate the downlink log against the limit file |
| 030  | EHMTRN  | Form the deterioration trends by control break   |
| 040  | EHMALR  | Raise exceedance alerts and rank the triage queue|
| 050  | EHMRUL  | Project remaining life and score fleet health    |
| 060  | MROPLN  | Allocate shop visit slots against the plan       |
| 070  | FLTRPT  | Print the fleet status report                    |

A non zero condition code flushes the remaining steps, as it does on the
mainframe.

## Directories

| Directory | Contents                                              |
|-----------|-------------------------------------------------------|
| `src`     | FORTRAN source decks, one program or module per file  |
| `inc`     | Include decks - installation parameters and `COMMON`  |
| `dat`     | Control card, parameter limit file, shop slot file    |
| `jcl`     | Production job deck and its open systems equivalent   |
| `bin`     | Load modules, written by the compile                  |
| `work`    | Work library, holds the master files and the print    |

## Compiling and running

```
make            compile and link every load module into BIN
make run        compile, link and execute the nightly job stream
make clean      delete object modules, load modules and work files
```

The legacy dialect switch in the makefile must not be removed. The source
is compiled with

```
gfortran -std=legacy -ffixed-form -ffixed-line-length-72 -fno-automatic
```

The print file is written to `work/SYSPRINT.TXT` and carries ANSI carriage
control in column one; `1` throws to a new page and `0` double spaces.
Error diagnostics are cut to `work/SYSERR.LOG`.

## Files

| Data set     | Written by | Read by                | Contents                       |
|--------------|------------|------------------------|--------------------------------|
| `ENGMAS.DAT` | EHMGEN     | 020 030 040 050 060 070| Engine master, serial sequence  |
| `FLTLOG.DAT` | EHMGEN     | EHMLOD                 | Raw downlink, one card a sector |
| `FLTVAL.DAT` | EHMLOD     | 030 040                | Validated downlink              |
| `TRNMAS.DAT` | EHMTRN     | 040 050 070            | Deterioration trends            |
| `ALRTMS.DAT` | EHMALR     | FLTRPT                 | Alert master                    |
| `RULMAS.DAT` | EHMRUL     | 060 070                | Life projection and health score|
| `SVPLAN.DAT` | MROPLN     | FLTRPT                 | Shop visit plan                 |
| `PARLIM.DAT` | Maintained | 020 040                | Parameter limits, amber and red |
| `SHPSLT.DAT` | Maintained | MROPLN                 | Shop slot capacity              |
| `EHMCTL.DAT` | Maintained | every step             | Run control card                |

Every record layout is declared once, in `src/ehmio.f`. No other program
may code a `READ` or a `WRITE` against a master file.

## Run control card

```
COL 01-08  RUN IDENTIFIER
COL 10-14  RUN DATE, JULIAN, FORM YYDDD
COL 16-20  TREND WINDOW IN FLIGHT CYCLES
COL 22-31  GENERATOR SEED
```

The seed drives the deterministic Lehmer generator in `RANF`, so a given
control card always reproduces the same fleet, the same downlink and the
same report.

## Table limits

Table sizes are fixed at compile time in `inc/EHMPRM.INC`.

| Parameter | Value | Table                       |
|-----------|-------|-----------------------------|
| `MAXENG`  | 240   | Engines on the master       |
| `MAXFLT`  | 24000 | Downlink records in a window|
| `MAXALR`  | 2000  | Alerts in one run           |
| `MAXSLT`  | 60    | Shop slots on file          |
| `MAXPRM`  | 12    | Monitored parameters        |
| `MAXMOD`  | 11    | Engine modules              |

An overflow is diagnosed and the table truncated; the run is not stopped.
