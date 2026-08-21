#!/bin/sh
#=====================================================================
# VERIFY   PROVE THAT THE JOB STREAM STILL BEHAVES BYTE FOR BYTE AS
#          THE COMMITTED GOLDEN BASELINE
#          ROLLS-ROYCE PLC - CIVIL AERO SUPPORT - DERBY
#
# RUNS THE SUITE FROM A COMPLETELY CLEAN STATE, COPIES THE ARTEFACTS
# INTO A TEMPORARY OUTPUT AREA AND DIFFS EACH ONE AGAINST TEST/GOLDEN.
# EXIT 0 MEANS NO DIFFERENCES, EXIT 8 MEANS AT LEAST ONE FILE DIFFERS
# AND THE OFFENDING FILES ARE NAMED ON THE REPORT, EXIT 12 MEANS THE
# COMPARISON COULD NOT BE MADE AT ALL.
#
# RUN THIS AFTER EVERY REFACTOR.  SEE TEST/README.MD.
#=====================================================================
set -e

BASE=`cd \`dirname $0\`/.. && pwd`
GOLDEN=$BASE/test/golden

. $BASE/tools/ehmfiles.sh

if [ ! -d $GOLDEN ] ; then
    echo "VERIFY: NO GOLDEN BASELINE AT $GOLDEN - RUN tools/baseline.sh" >&2
    exit 12
fi

OUT=`mktemp -d`
trap 'rm -rf $OUT' 0 1 2 3 15

cd $BASE

echo "VERIFY: CLEAN BUILD AND RUN"
rm -rf work

# ---- A BAD BUILD, OR A STEP THAT ABENDS AND FLUSHES THE REST OF THE --
# ---- JOB STREAM, LEAVES NOTHING TO COMPARE.  SAY SO AND REPORT THE ---
# ---- DOCUMENTED CONDITION CODE INSTEAD OF DYING UNDER SET -E. -------
set +e
make clean && make && make run
RC=$?
set -e
if [ $RC -ne 0 ] ; then
    echo "VERIFY: BUILD OR JOB STREAM FAILED - MAKE RC $RC - NOTHING TO COMPARE" >&2
    echo "VERIFY: SEE work/SYSPRINT.TXT AND work/SYSERR.LOG" >&2
    exit 12
fi

echo "VERIFY: COPYING ARTEFACTS INTO $OUT"
for F in $EHM_FILES ; do
    if [ ! -f work/$F ] ; then
        echo "VERIFY: EXPECTED FILE work/$F WAS NOT PRODUCED" >&2
        exit 12
    fi
    cp work/$F $OUT/$F
done

echo "VERIFY: COMPARING AGAINST $GOLDEN"
NDIFF=0
for F in $EHM_FILES ; do
    if [ ! -f $GOLDEN/$F ] ; then
        echo "VERIFY: MISSING FROM BASELINE - $F"
        NDIFF=`expr $NDIFF + 1`
    elif cmp -s $GOLDEN/$F $OUT/$F ; then
        echo "VERIFY: SAME      - $F"
    else
        echo "VERIFY: DIFFERENT - $F"
        NDIFF=`expr $NDIFF + 1`
    fi
done

# ---- ANY EXTRA BASELINED FILE THAT THE RUN NO LONGER PRODUCES -------
for G in $GOLDEN/* ; do
    F=`basename $G`
    if [ ! -f $OUT/$F ] ; then
        echo "VERIFY: NOT PRODUCED - $F"
        NDIFF=`expr $NDIFF + 1`
    fi
done

if [ $NDIFF -ne 0 ] ; then
    echo "VERIFY: $NDIFF FILE(S) DIFFER FROM THE GOLDEN BASELINE" >&2
    echo "VERIFY: INSPECT WITH  diff test/golden/<FILE> work/<FILE>" >&2
    exit 8
fi

echo "VERIFY: ZERO DIFFERENCES - BEHAVIOUR PRESERVED"
