---
name: testing-ehms-batch
description: How to build, run and end-to-end verify the EHMS FORTRAN 77 batch suite (rr-legacy-fortran) - determinism, cross-file coherence and adversarial control-card testing. Use when testing any change to src/, inc/, dat/, jcl/ or the Makefile.
---

# Testing the EHMS 4.2 batch suite

There is **no UI and no server**. Everything is a CLI batch job stream, so test with the
shell tool only and do **not** start a screen recording — this box has no terminal emulator
installed (no `xterm`/`gnome-terminal`), only `wmctrl`/`xdotool`, so a GUI terminal is not
an option either. Evidence = captured command output.

## Devin Secrets Needed
None. Everything runs locally; `gfortran` is installed by the repo blueprint's `initialize`.

## Build and run

```
make clean && make        # 7 load modules into bin/, must be warning/error free
make run                  # runs jcl/RUNEHM.SH -> steps 010..070
```

Expect 7 pairs of `EHM0001I STEP nnn ... STARTED` / `EHM0002I STEP nnn ... ENDED COND CODE 0`
then `EHM0003I JOB ENDED`. Print file `work/SYSPRINT.TXT`, diagnostics `work/SYSERR.LOG`.

## Gotchas learned the hard way

- **`work/` is not cleared by the job stream.** `RUNEHM.SH` only does `mkdir -p work`. Always
  `rm -rf work` before a comparison run, otherwise stale masters and a stale `SYSERR.DAT`
  contaminate results.
- `SYSERR.DAT` is scratched and reallocated per step (`STATUS='REPLACE'`, and `RUNEHM.SH`
  removes it before each step), so `SYSERR.LOG` counts are real event counts and are
  deterministic. If you see duplicated diagnostics, a stale `work/` is the cause.
- A failing step prints `EHM0002I ... COND CODE nn` followed by `EHM0004E ... ABENDED -
  REMAINING STEPS FLUSHED`, and the job stream exits with that condition code.
- Serial numbers are `ESN-nnnn` (contain a hyphen) — regexes like `[A-Z0-9]{8}` will not match
  report rows.
- Print lines carry ANSI carriage control in column 1: `1` for a page header, `0` for a
  double-spaced line, blank for a single-spaced line. Strip column 1 before parsing report
  rows.
- `****` (4 stars) in the slot/shop columns is the legitimate "unallocated" placeholder; a
  FORMAT overflow would show 5+ stars in a numeric column, so grep for `\*\*\*\*\*`.
- The control card's trend window (`JWINDW`, clamped to a minimum of 50) caps the number of
  sectors EHMTRN reduces per engine, oldest discarded first. The generator writes about 66
  sectors per engine, so any window at or above that leaves `TRNMAS.DAT` unchanged — probe
  with a window of 50, or change the **seed** for a completely different fleet.

## Coherence harness

A reusable assertion script lives at `/home/ubuntu/ehms-test/coherence.sh` (recreate if absent).
It cross-checks Section 5 statistics against actual record counts in
`ENGMAS/RULMAS/TRNMAS/ALRTMS/SVPLAN.DAT`, checks orphan serials, checks Section 2 RUL/status
against `RULMAS.DAT`, and checks that no shop slot exceeds its bay capacity (SHPSLT col 16-17)
and no engine is placed in a slot whose capability is neither `FULL` nor its own workscope.

## Adversarial cases worth re-running

Mutate `dat/EHMCTL.DAT`, `dat/PARLIM.DAT`, `dat/SHPSLT.DAT`, run, then
`git checkout -- dat/`. Expected diagnostics:

| Mutation | Expected |
|---|---|
| empty / garbage control card | `EHM1001E` + installation defaults, all steps RC 0 |
| trend window < 50 | clamped to 50 in Section 5, and `TRNMAS.DAT` differs from baseline |
| PARLIM malformed card | `EHM1022E`, run continues |
| PARLIM empty | `EHM1020E`, alerts drop, counts stay coherent |
| PARLIM > MAXPRM(12) cards | `EHM1021E` |
| SHPSLT malformed card | `EHM1062E`, slot absent from loading table |
| SHPSLT empty | `EHM1060E`, `SLOTS ON FILE 0`, all engines unallocated |
| SHPSLT > MAXSLT(60) cards | `EHM1061E`, `SLOTS ON FILE 60` |
| control file missing | `EHM1002E OPEN FAILED` + `STOP 16` from the program; the shell script itself dies at the `cp` |
| output data set write protected | `EHM1002E OPEN FAILED` + `STOP 16`, then `EHM0004E ... ABENDED` |

**Always finish with `git checkout -- dat/` and confirm `git status --porcelain` is empty.**
