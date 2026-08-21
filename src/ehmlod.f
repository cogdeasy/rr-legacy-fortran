C=====================================================================
C     EHMLOD   STEP 020 - DOWNLINK VALIDATION AND LOAD
C
C     READS THE RAW DOWNLINK LOG, APPLIES THE EDIT RULES OF DOCUMENT
C     EHM/VR/002 AND WRITES A VALIDATED LOG FOR THE ANALYSIS STEPS.
C     A RECORD IS REJECTED WHEN
C
C        A.  THE SERIAL NUMBER IS NOT ON THE ENGINE MASTER
C        B.  THE OBSERVATION DATE FALLS OUTSIDE THE REPORTING WINDOW
C        C.  A GAS PATH OR OIL SYSTEM READING IS OUTSIDE THE
C            PHYSICALLY CREDIBLE ENVELOPE
C
C     REJECTED RECORDS ARE LISTED ON THE ERROR FILE AND COUNTED BY
C     REASON.  THE STEP ALSO PRINTS THE TELEMETRY COVERAGE REPORT,
C     WHICH TELLS THE CONTROLLER WHETHER A RED FLAG LATER IN THE JOB
C     STREAM IS SUPPORTED BY ENOUGH DATA TO BE TRUSTED.
C
C     FILES     ENGMAS  INPUT   FLTLOG  INPUT
C               FLTVAL  OUTPUT  SYSERR  OUTPUT
C=====================================================================
      PROGRAM EHMLOD
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      INTEGER NCOV(MAXENG)
      INTEGER I, J, IRC, JD, JLOW, NIN, NOUT, NBAD
      INTEGER NRJSER, NRJDAT, NRJRNG, NZERO, NTHIN, NEXP, IPAGE
      INTEGER NLINE
      INTEGER ENGFND, JULADD
      REAL V(NFVAL), COVPCT
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
      CHARACTER*5 CSTAT
      EXTERNAL ENGFND, JULADD, CSTAT
C
C     ================================================================
C     STEP 1.  INITIALISE
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      CALL LIMLOD
      IPAGE = 0
      NLINE = 99
C
      DO 100 I = 1, NENG
         NCOV(I) = 0
  100 CONTINUE
C
      NIN = 0
      NOUT = 0
      NBAD = 0
      NRJSER = 0
      NRJDAT = 0
      NRJRNG = 0
      JLOW = JULADD(JRUNDT, -400)
C
C     ================================================================
C     STEP 2.  PASS THE RAW LOG AND APPLY THE EDIT RULES
C     ================================================================
      CALL OPNSEQ (LUFLT, 'FLTLOG.DAT', 'OLD')
      CALL OPNSEQ (LUVAL, 'FLTVAL.DAT', 'NEW')
C
  200 CONTINUE
      CALL FLTRD (LUFLT, CE, JD, CFLT, CORG, CDST, V, IRC)
      IF (IRC .EQ. KEOF) GO TO 400
      IF (IRC .EQ. KBAD) GO TO 300
      NIN = NIN + 1
C
C     ---- RULE A.  SERIAL MUST BE ON THE ENGINE MASTER ---------------
      J = ENGFND(CE)
      IF (J .NE. 0) GO TO 210
      NRJSER = NRJSER + 1
      WRITE (LUERR,9500) CE, JD, 'SERIAL NOT ON ENGINE MASTER'
      GO TO 200
C
C     ---- RULE B.  OBSERVATION DATE WITHIN THE WINDOW ----------------
  210 CONTINUE
      IF (JD .LE. JRUNDT .AND. JD .GE. JLOW) GO TO 220
      NRJDAT = NRJDAT + 1
      WRITE (LUERR,9500) CE, JD, 'DATE OUTSIDE REPORTING WINDOW'
      GO TO 200
C
C     ---- RULE C.  READINGS PHYSICALLY CREDIBLE ----------------------
  220 CONTINUE
      IF (V(4) .LT. 250.0 .OR. V(4) .GT. 1250.0) GO TO 250
      IF (V(5) .LT. 0.0 .OR. V(5) .GT. 200.0) GO TO 250
      IF (V(6) .LT. 0.0 .OR. V(6) .GT. 125.0) GO TO 250
      IF (V(7) .LT. 0.0 .OR. V(7) .GT. 125.0) GO TO 250
      IF (V(9) .LT. 0.0 .OR. V(9) .GT. 20.0) GO TO 250
      IF (V(12) .LT. 0.0 .OR. V(12) .GT. 150.0) GO TO 250
      IF (V(1) .LT. 0.1 .OR. V(1) .GT. 20.0) GO TO 250
      GO TO 260
  250 CONTINUE
      NRJRNG = NRJRNG + 1
      WRITE (LUERR,9500) CE, JD, 'READING OUTSIDE CREDIBLE ENVELOPE'
      GO TO 200
C
C     ---- RECORD ACCEPTED --------------------------------------------
  260 CONTINUE
      CALL FLTWR (LUVAL, CE, JD, CFLT, CORG, CDST, V)
      NCOV(J) = NCOV(J) + 1
      NOUT = NOUT + 1
      GO TO 200
C
  300 CONTINUE
      NBAD = NBAD + 1
      CALL ERRMSG (2010, 'UNREADABLE DOWNLINK RECORD SKIPPED')
      GO TO 200
C
  400 CONTINUE
      CLOSE (LUFLT)
      CLOSE (LUVAL)
C
C     ================================================================
C     STEP 3.  TELEMETRY COVERAGE REPORT
C     ================================================================
      NZERO = 0
      NTHIN = 0
      NEXP = 0
      CALL PAGHDR (LUPRT, 'TELEMETRY COVERAGE - STEP 020', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9000)
C
      DO 500 I = 1, NENG
C        AN ENGINE STRIPPED IN THE SHOP IS NOT EXPECTED TO REPORT AND
C        IS KEPT OUT OF THE COVERAGE PERCENTAGE.
         IF (ISTG(I) .EQ. 4) GO TO 440
         NEXP = NEXP + 1
         IF (NCOV(I) .EQ. 0) NZERO = NZERO + 1
         IF (NCOV(I) .GT. 0 .AND. NCOV(I) .LT. 20) NTHIN = NTHIN + 1
  440    CONTINUE
         IF (NCOV(I) .GE. 20) GO TO 500
         IF (NLINE .LT. MAXLIN) GO TO 450
         CALL PAGHDR (LUPRT, 'TELEMETRY COVERAGE - STEP 020', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9000)
  450    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CFAM(I), COPR(I), CTAIL(I),
     +                      CLOCN(I), ISTG(I), NCOV(I)
         NLINE = NLINE + 1
  500 CONTINUE
C
      COVPCT = 0.0
      IF (NEXP .GT. 0) COVPCT = 100.0 * FLOAT(NEXP - NZERO)
     +                          / FLOAT(NEXP)
      WRITE (LUPRT,9020) NIN, NOUT, NRJSER, NRJDAT, NRJRNG, NBAD,
     +                   NENG, NEXP, NZERO, NTHIN, COVPCT
C
      IF (COVPCT .LT. 90.0) CALL ERRMSG (2011,
     +   'FLEET COVERAGE BELOW NINETY PER CENT - REVIEW GATEWAY')
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('0ENGINES WITH THIN OR ABSENT COVERAGE' /
     +        '0SERIAL   FAM  OPR TAIL   STN  STAGE  SECTORS' /
     +        ' ', 50('-'))
 9010 FORMAT (' ', A8, 1X, A4, 1X, A2, 2X, A6, 1X, A4, 3X, I1,
     +        5X, I5)
 9020 FORMAT ('0', 50('-') /
     +        ' RECORDS READ                   ', I8 /
     +        ' RECORDS ACCEPTED               ', I8 /
     +        ' REJECT - UNKNOWN SERIAL        ', I8 /
     +        ' REJECT - DATE OUT OF WINDOW    ', I8 /
     +        ' REJECT - READING NOT CREDIBLE  ', I8 /
     +        ' REJECT - RECORD UNREADABLE     ', I8 /
     +        ' ENGINES ON MASTER              ', I8 /
     +        ' ENGINES EXPECTED TO REPORT     ', I8 /
     +        ' ENGINES WITH NO DOWNLINK       ', I8 /
     +        ' ENGINES WITH THIN COVERAGE     ', I8 /
     +        ' FLEET COVERAGE PER CENT        ', F8.1 /
     +        '0EHM2020I STEP 020 COMPLETE')
 9500 FORMAT (' EHM2012E ', A8, 1X, I5, 1X, A)
      END
