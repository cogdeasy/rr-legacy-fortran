C=====================================================================
C     EHMTRN   STEP 030 - PERFORMANCE TREND ANALYSIS
C
C     FORMS, FOR EVERY ENGINE ON THE MASTER, THE DETERIORATION TREND
C     OF EXHAUST GAS TEMPERATURE MARGIN AGAINST CYCLES FLOWN, AND THE
C     SUPPORTING CONDITION INDICATORS TAKEN FROM THE SAME WINDOW
C
C        SLOPE   DEGREES C OF MARGIN LOST PER FLIGHT CYCLE
C        RSQ     GOODNESS OF FIT - A POOR FIT MEANS THE SLOPE MUST
C                NOT BE USED TO PROJECT A REMOVAL DATE
C        VBMAX   WORST BROADBAND VIBRATION SEEN ON THE FAN SHAFT
C        OILC    MEAN OIL CONSUMPTION IN LITRES PER HOUR
C        DEBRIS  MEAN MAGNETIC DEBRIS PARTICLE COUNT
C        SFCD    SPECIFIC FUEL CONSUMPTION DEVIATION, PER CENT,
C                AGAINST THE BUILD STANDARD DATUM FOR THE FAMILY
C
C     THE VALIDATED LOG IS IN SERIAL NUMBER SEQUENCE, SO THE ENGINE
C     TOTALS ARE ACCUMULATED BY CONTROL BREAK AND ONLY ONE ENGINE
C     WINDOW IS EVER HELD IN CORE.
C
C     THE WINDOW LENGTH IS TAKEN FROM THE RUN CONTROL CARD.  WHEN AN
C     ENGINE HAS FLOWN MORE SECTORS THAN THE WINDOW ALLOWS, THE
C     OLDEST SECTOR IS DISCARDED AND THE TREND IS FORMED ON THE MOST
C     RECENT SECTORS ONLY.
C
C     FILES     ENGMAS  INPUT   FLTVAL  INPUT
C               TRNMAS  OUTPUT  SYSERR  OUTPUT
C=====================================================================
      PROGRAM EHMTRN
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      INTEGER MAXOBS, NFAM
      PARAMETER (MAXOBS = 400, NFAM = 6)
C
      REAL X(MAXOBS), Y(MAXOBS)
      REAL YVIB(MAXOBS), YOIL(MAXOBS), YDEB(MAXOBS)
      REAL YFUL(MAXOBS), YHRS(MAXOBS)
      CHARACTER*4 CFTAB(NFAM)
      REAL FFDATM(NFAM)
      DATA CFTAB /'TXWA', 'TXWB', 'T10T', 'T700', 'T900', 'UFAN'/
      DATA FFDATM /2680.0, 2910.0, 2740.0, 2620.0, 3120.0, 2480.0/
C
      INTEGER I, J, K, JCUR, IRC, JD, NOBS, NCAP, NENGT, NSHRT
      INTEGER IPAGE, NLINE, NSTEEP
      INTEGER ENGFND
      REAL V(NFVAL)
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
      EXTERNAL ENGFND
C
C     ================================================================
C     STEP 1.  INITIALISE
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      IPAGE = 0
      NLINE = 99
      NENGT = 0
      NSHRT = 0
      NSTEEP = 0
C
      DO 100 I = 1, NENG
         NTOBS(I) = 0
         TSLOPE(I) = 0.0
         TINTC(I) = 0.0
         TRSQ(I) = 0.0
         TVBMX(I) = 0.0
         TOILC(I) = 0.0
         TDEBR(I) = 0.0
         TSFCD(I) = 0.0
  100 CONTINUE
C
      NCAP = JWINDW
      IF (NCAP .GT. MAXOBS) NCAP = MAXOBS
C
      CALL OPNSEQ (LUVAL, 'FLTVAL.DAT', 'OLD')
      JCUR = 0
      NOBS = 0
C
C     ================================================================
C     STEP 2.  ACCUMULATE THE WINDOW, BREAKING ON SERIAL NUMBER
C     ================================================================
  200 CONTINUE
      CALL FLTRD (LUVAL, CE, JD, CFLT, CORG, CDST, V, IRC)
      IF (IRC .EQ. KEOF) GO TO 500
      IF (IRC .EQ. KBAD) GO TO 200
      J = ENGFND(CE)
      IF (J .EQ. 0) GO TO 200
C
      IF (JCUR .EQ. 0) JCUR = J
      IF (J .EQ. JCUR) GO TO 300
C
C     ---- CONTROL BREAK - REDUCE THE WINDOW JUST ACCUMULATED ---------
      CALL TRNRED (JCUR, X, Y, YVIB, YOIL, YDEB, YFUL, YHRS, NOBS,
     +             CFTAB, FFDATM, NFAM)
      NENGT = NENGT + 1
      JCUR = J
      NOBS = 0
C
  300 CONTINUE
      IF (NOBS .LT. NCAP) GO TO 350
C
C     ---- WINDOW FULL - DROP THE OLDEST SECTOR -----------------------
      DO 320 K = 2, NCAP
         Y(K-1) = Y(K)
         YVIB(K-1) = YVIB(K)
         YOIL(K-1) = YOIL(K)
         YDEB(K-1) = YDEB(K)
         YFUL(K-1) = YFUL(K)
         YHRS(K-1) = YHRS(K)
  320 CONTINUE
      NOBS = NCAP - 1
  350 CONTINUE
      NOBS = NOBS + 1
      X(NOBS) = FLOAT(NOBS)
      Y(NOBS) = V(5)
      YVIB(NOBS) = V(9)
      YOIL(NOBS) = V(14)
      YDEB(NOBS) = V(16)
      YFUL(NOBS) = V(15)
      YHRS(NOBS) = V(1)
      GO TO 200
C
C     ---- END OF FILE - REDUCE THE LAST WINDOW -----------------------
  500 CONTINUE
      IF (JCUR .EQ. 0) GO TO 550
      CALL TRNRED (JCUR, X, Y, YVIB, YOIL, YDEB, YFUL, YHRS, NOBS,
     +             CFTAB, FFDATM, NFAM)
      NENGT = NENGT + 1
  550 CONTINUE
      CLOSE (LUVAL)
C
C     ================================================================
C     STEP 3.  WRITE THE TREND MASTER AND LIST THE STEEPEST DECAYS
C     ================================================================
      CALL OPNSEQ (LUTRN, 'TRNMAS.DAT', 'NEW')
      DO 600 I = 1, NENG
         CALL TRNWR (LUTRN, I)
         IF (NTOBS(I) .GT. 0 .AND. NTOBS(I) .LT. 20) NSHRT = NSHRT + 1
  600 CONTINUE
      CLOSE (LUTRN)
C
      CALL PAGHDR (LUPRT, 'DETERIORATION TRENDS - STEP 030', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9000)
      DO 700 I = 1, NENG
         IF (NTOBS(I) .LT. 20) GO TO 700
         IF (TSLOPE(I) .GT. -0.0090) GO TO 700
         IF (TRSQ(I) .LT. 0.25) GO TO 700
         NSTEEP = NSTEEP + 1
         IF (NLINE .LT. MAXLIN) GO TO 650
         CALL PAGHDR (LUPRT, 'DETERIORATION TRENDS - STEP 030',
     +                IPAGE, NLINE)
         WRITE (LUPRT,9000)
  650    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CFAM(I), COPR(I), IENVS(I),
     +                      EGTM(I), TSLOPE(I), TRSQ(I), NTOBS(I),
     +                      TVBMX(I), TOILC(I), TDEBR(I), TSFCD(I)
         NLINE = NLINE + 1
  700 CONTINUE
C
      WRITE (LUPRT,9020) NENG, NENGT, NSHRT, NSTEEP
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('0ENGINES DETERIORATING FASTER THAN NINE THOUSANDTHS ',
     +        'OF A DEGREE PER CYCLE' /
     +        '0SERIAL   FAM  OPR ENV   EGTM   SLOPE/CYC   RSQ',
     +        '  OBS  VIBMAX  OILC/H  DEBRIS   SFC%' /
     +        ' ', 100('-'))
 9010 FORMAT (' ', A8, 1X, A4, 1X, A2, 3X, I1, 1X, F6.1, 3X,
     +        F9.5, 1X, F5.3, 1X, I4, 2X, F6.2, 2X, F6.3, 2X,
     +        F6.1, 1X, F6.2)
 9020 FORMAT ('0', 100('-') /
     +        ' ENGINES ON MASTER              ', I8 /
     +        ' ENGINE WINDOWS REDUCED         ', I8 /
     +        ' WINDOWS TOO SHORT TO PROJECT   ', I8 /
     +        ' ENGINES ON STEEP DECAY         ', I8 /
     +        '0EHM2030I STEP 030 COMPLETE')
      END
C
      SUBROUTINE TRNRED (J, X, Y, YVIB, YOIL, YDEB, YFUL, YHRS,
     +                   NOBS, CFTAB, FFDATM, NFAM)
C     ----------------------------------------------------------------
C     REDUCE ONE ENGINE WINDOW INTO THE TREND TABLE.  THE FIT IS
C     REJECTED, AND THE SLOPE FORCED FLAT, WHEN TOO FEW SECTORS WERE
C     DOWNLINKED FOR THE RESULT TO CARRY ANY WEIGHT.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER J, NOBS, NFAM
      REAL X(*), Y(*), YVIB(*), YOIL(*), YDEB(*), YFUL(*), YHRS(*)
      REAL FFDATM(NFAM)
      CHARACTER*4 CFTAB(NFAM)
      INTEGER K, IFAM
      REAL SLOPE, YINT, RSQ, FFACT
      REAL VBMX, SOILC, SDEBR, SFUEL, SHOUR
C
      IF (J .LE. 0 .OR. J .GT. MAXENG) RETURN
      NTOBS(J) = NOBS
      IF (NOBS .LE. 0) RETURN
C
C     ---- TOTAL THE CONDITION INDICATORS OVER THE WINDOW -------------
      VBMX = 0.0
      SOILC = 0.0
      SDEBR = 0.0
      SFUEL = 0.0
      SHOUR = 0.0
      DO 50 K = 1, NOBS
         IF (YVIB(K) .GT. VBMX) VBMX = YVIB(K)
         SOILC = SOILC + YOIL(K)
         SDEBR = SDEBR + YDEB(K)
         SFUEL = SFUEL + YFUL(K)
         SHOUR = SHOUR + YHRS(K)
   50 CONTINUE
C
      CALL LINREG (X, Y, NOBS, SLOPE, YINT, RSQ)
      IF (NOBS .GE. 12) GO TO 100
      SLOPE = 0.0
      RSQ = 0.0
      CALL ERRMSG (2031, 'TREND WINDOW TOO SHORT - SLOPE SUPPRESSED')
  100 CONTINUE
      TSLOPE(J) = SLOPE
      TINTC(J) = YINT
      TRSQ(J) = RSQ
      TVBMX(J) = VBMX
      TOILC(J) = SOILC / FLOAT(NOBS)
      TDEBR(J) = SDEBR / FLOAT(NOBS)
C
C     ---- SPECIFIC FUEL CONSUMPTION DEVIATION ------------------------
      TSFCD(J) = 0.0
      IF (SHOUR .LT. 1.0) RETURN
      IFAM = 1
      DO 200 K = 1, NFAM
         IF (CFAM(J) .EQ. CFTAB(K)) IFAM = K
  200 CONTINUE
      FFACT = SFUEL / SHOUR
      TSFCD(J) = 100.0 * (FFACT - FFDATM(IFAM)) / FFDATM(IFAM)
      RETURN
      END
