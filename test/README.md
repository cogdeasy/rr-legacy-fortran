# EHMS golden-baseline regression harness

The suite is deterministic: `dat/EHMCTL.DAT` fixes the run identifier, the julian run
date, the trend window and the `JSEED` value that drives the `RANF` Lehmer generator in
`src/ehmlib.f`. With unchanged inputs, a clean build-and-run therefore reproduces the
print file and every master file byte for byte. That is what this harness locks down, so
later modernization phases can prove they preserved behaviour.

## What is baselined

`test/golden/` holds the artefacts of one clean run of `jcl/RUNEHM.SH`:

| File | Written by |
|---|---|
| `ENGMAS.DAT` | step 010 EHMGEN |
| `FLTLOG.DAT` | step 010 EHMGEN |
| `FLTVAL.DAT` | step 020 EHMLOD |
| `TRNMAS.DAT` | step 030 EHMTRN |
| `ALRTMS.DAT` | step 040 EHMALR |
| `RULMAS.DAT` | step 050 EHMRUL |
| `SVPLAN.DAT` | step 060 MROPLN |
| `SYSPRINT.TXT` | all steps (job print file) |
| `SYSERR.LOG` | all steps (accumulated diagnostics) |

The control cards that `RUNEHM.SH` copies into `work/` (`EHMCTL.DAT`, `PARLIM.DAT`,
`SHPSLT.DAT`) are inputs and are not baselined. `work/SYSERR.DAT` is scratched and
reallocated by every step, so only its accumulation in `SYSERR.LOG` is kept.

The file list lives in one place, `tools/ehmfiles.sh`, sourced by both scripts.

Determinism is a property of the inputs, not of the machine: report values are floating
point computed at `-O1`, so the baseline is only reproducible on the same compiler and
platform. Re-baseline after a toolchain change, and treat such a diff as toolchain noise
rather than evidence about the source change.

## Verify after each refactor

```sh
./tools/verify.sh
```

It removes `work/`, does `make clean && make && make run`, copies the artefacts into a
temporary output area and compares each one with `test/golden/`. It prints one
`SAME`/`DIFFERENT` line per file and exits

* `0` — zero differences, behaviour preserved;
* `8` — at least one file differs (or is missing/no longer produced), each named on the report;
* `12` — no baseline present, or the run did not produce an expected file.

`work/` still holds the run that was just compared, so a reported difference can be
inspected directly:

```sh
diff test/golden/SYSPRINT.TXT work/SYSPRINT.TXT
cmp -l test/golden/TRNMAS.DAT work/TRNMAS.DAT | head
```

A clean `work/` matters: `jcl/RUNEHM.SH` only does `mkdir -p work`, it never purges it, so
a run over stale masters and a stale `SYSERR.LOG` will not compare meaningfully. Both
scripts do the `rm -rf work` for you — never compare a run you started with `make run` by
hand on a dirty tree.

## Regenerate the baseline

Only when an **intentional** change of behaviour has been made and reviewed — never to
make a failing `verify.sh` go quiet.

```sh
./tools/baseline.sh
./tools/verify.sh          # must report zero differences
git add test/golden && git commit
```

Review the diff of `test/golden/` in the commit as carefully as the source change itself:
it is the evidence of what the behaviour change actually did. A refactor that is meant to
be behaviour-preserving must never carry a `test/golden/` diff.

Changing `dat/EHMCTL.DAT` (seed, run date, trend window) or any other control card also
changes the artefacts and so requires a deliberate re-baseline.
