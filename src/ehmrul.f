C=====================================================================
C     EHMRUL   STEP 050 - LIFE PROJECTION AND FLEET HEALTH SCORING
C
C     PROJECTS, FOR EVERY ENGINE, THE NUMBER OF FLIGHT CYCLES THAT
C     MAY STILL BE FLOWN BEFORE REMOVAL IS FORCED.  TWO INDEPENDENT
C     LIMITS ARE EVALUATED AND THE LOWER IS TAKEN
C
C        A.  PERFORMANCE LIMIT.  THE MEASURED DETERIORATION SLOPE IS
C            PROJECTED FORWARD UNTIL THE EGT MARGIN REACHES THE
C            MINIMUM DISPATCH MARGIN OF THREE DEGREES.  WHERE THE
C            SLOPE IS FLAT OR THE FIT IS POOR THE FLEET MEAN DECAY
C            RATE FOR THE ROUTE ENVIRONMENT IS SUBSTITUTED.
C
C        B.  MODULE LIFE LIMIT.  THE CERTIFIED CYCLIC LIFE OF EACH
C            MODULE, DERATED FOR ROUTE SEVERITY, LESS THE CYCLES
C            RUN SINCE THE LAST OVERHAUL.  THE MODULE WITH THE LEAST
C            REMAINING LIFE IS REPORTED AS THE LIMITING MODULE AND
C            DRIVES THE WORKSCOPE CHOSEN BY THE PLANNING STEP.
C
C     A FLEET RELATIVE HEALTH SCORE OF NOUGHT TO ONE HUNDRED AND THE
C     RED, AMBER OR GREEN ROLL UP ARE FORMED AT THE SAME TIME.
C
C     FILES     ENGMAS  INPUT   TRNMAS  INPUT
C               RULMAS  OUTPUT  SYSERR  OUTPUT
C=====================================================================
      PROGRAM EHMRUL
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      REAL CERTLF(MAXMOD)
      INTEGER JMOD(MAXENG), JCYC(MAXENG), INDX(MAXENG)
      REAL RISK(MAXENG), RKEY(MAXENG)
C
C     ---- CERTIFIED CYCLIC LIFE BY MODULE, BENIGN ENVIRONMENT --------
C          FAN  IPC  HPC  COMB  HPT  IPT  LPT  GBOX ACCY NACL EXTL
      DATA CERTLF /25000.0, 22000.0, 18000.0, 12000.0, 9000.0,
     +             14000.0, 16000.0, 20000.0, 15000.0, 30000.0,
     +             20000.0/
C
      INTEGER I, M, IPAGE, NLINE, NRED, NAMB, NGRN, NGRY, NLIM
      INTEGER NPERF, NMODL, NRUL
      REAL SLOPE, DECAY, RPERF, RMODL, RLIFE, RREM, SCORE, PEN
      REAL SEVFAC, DMIN
      CHARACTER*5 CSTAT
      CHARACTER*9 CMODUL
      EXTERNAL CSTAT, CMODUL
C
C     ---- MINIMUM DISPATCH MARGIN IN DEGREES C -----------------------
      DATA DMIN /3.0/
C
C     ================================================================
C     STEP 1.  LOAD THE MASTERS
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      CALL TRNLOD
      IPAGE = 0
      NLINE = 99
      NPERF = 0
      NMODL = 0
C
C     ================================================================
C     STEP 2.  PROJECT EACH ENGINE
C     ================================================================
      DO 300 I = 1, NENG
C
C        ---- A.  PERFORMANCE LIMITED LIFE ----------------------------
         SLOPE = TSLOPE(I)
         IF (NTOBS(I) .GE. 20 .AND. TRSQ(I) .GE. 0.30 .AND.
     +       SLOPE .LT. -0.0005) GO TO 100
C        FLEET MEAN DECAY FOR THE ROUTE ENVIRONMENT
         SLOPE = -(0.0042 + 0.0013 * FLOAT(IENVS(I) - 1))
  100    CONTINUE
         DECAY = -SLOPE
         RPERF = (EGTM(I) - DMIN) / DECAY
         IF (RPERF .LT. 0.0) RPERF = 0.0
C
C        ---- B.  MODULE LIMITED LIFE ---------------------------------
         SEVFAC = 1.0 - 0.075 * FLOAT(IENVS(I) - 1)
         IF (SEVFAC .LT. 0.55) SEVFAC = 0.55
         RMODL = 1.0E9
         NLIM = 1
         DO 200 M = 1, MAXMOD
            RLIFE = CERTLF(M) * SEVFAC
            RREM = RLIFE - FLOAT(ICSO(I))
C           THE FAN DISC AND THE NACELLE STRUCTURE ARE LIFE LIMITED
C           PARTS.  THEIR CERTIFIED LIFE IS AN ABSOLUTE CYCLIC COUNT
C           FROM NEW AND IS NOT DERATED FOR ROUTE SEVERITY.
            IF (M .EQ. 1 .OR. M .EQ. 10)
     +         RREM = CERTLF(M) - FLOAT(ITFC(I))
            IF (RREM .LT. 0.0) RREM = 0.0
            IF (RREM .GE. RMODL) GO TO 200
            RMODL = RREM
            NLIM = M
  200    CONTINUE
C
C        ---- TAKE THE BINDING LIMIT ----------------------------------
         JMOD(I) = NLIM
         JCYC(I) = NINT(RMODL)
         NRUL = NINT(RPERF)
         IF (RMODL .LT. RPERF) NRUL = NINT(RMODL)
         IF (RMODL .LT. RPERF) NMODL = NMODL + 1
         IF (RMODL .GE. RPERF) NPERF = NPERF + 1
         IF (NRUL .GT. 99999) NRUL = 99999
         IRUL(I) = NRUL
C
C        ---- FLEET RELATIVE HEALTH SCORE -----------------------------
C        THE SCORE STARTS AT ONE HUNDRED AND IS PENALISED FOR EACH
C        CONDITION INDICATOR THAT IS AWAY FROM ITS DATUM.  IT IS A
C        RANKING DEVICE FOR THE CONTROLLER, NOT AN AIRWORTHINESS
C        JUDGEMENT, AND IS NEVER USED TO CLEAR AN ENGINE FOR FLIGHT.
         SCORE = 100.0
         PEN = 0.0
         IF (EGTM(I) .LT. 25.0) PEN = PEN + (25.0 - EGTM(I)) * 1.6
         IF (TVBMX(I) .GT. 2.0) PEN = PEN + (TVBMX(I) - 2.0) * 6.0
         IF (TOILC(I) .GT. 0.40) PEN = PEN + (TOILC(I) - 0.40) * 30.0
         IF (TDEBR(I) .GT. 12.0) PEN = PEN + (TDEBR(I) - 12.0) * 0.8
         IF (TSFCD(I) .GT. 1.0) PEN = PEN + (TSFCD(I) - 1.0) * 3.0
         IF (NRUL .LT. 1500) PEN = PEN + FLOAT(1500 - NRUL) * 0.012
         SCORE = SCORE - PEN
         IF (SCORE .LT. 0.0) SCORE = 0.0
         IF (SCORE .GT. 100.0) SCORE = 100.0
         HSCOR(I) = SCORE
C
C        ---- ROLL UP STATUS ------------------------------------------
         ISTAT(I) = KGREEN
         IF (SCORE .LT. 70.0 .OR. NRUL .LT. 900) ISTAT(I) = KAMBER
         IF (SCORE .LT. 45.0 .OR. NRUL .LT. 350) ISTAT(I) = KRED
         IF (EGTM(I) .LE. 5.0) ISTAT(I) = KRED
         IF (NTOBS(I) .EQ. 0) ISTAT(I) = KGREY
         IF (ISTG(I) .EQ. 4) ISTAT(I) = KGREY
C
C        ---- PROBABILITY OF REMOVAL WITHIN THREE HUNDRED CYCLES ------
         RISK(I) = 0.0
         IF (NRUL .GE. 3000) GO TO 300
         RISK(I) = 1.0 - FLOAT(NRUL) / 3000.0
         RISK(I) = RISK(I) ** 3
         IF (RISK(I) .GT. 0.9999) RISK(I) = 0.9999
  300 CONTINUE
C
C     ================================================================
C     STEP 3.  WRITE THE LIFE PROJECTION FILE
C     ================================================================
      CALL OPNSEQ (LURUL, 'RULMAS.DAT', 'NEW')
      DO 400 I = 1, NENG
         CALL RULWR (LURUL, I, JMOD(I), JCYC(I), RISK(I))
  400 CONTINUE
      CLOSE (LURUL)
C
C     ================================================================
C     STEP 4.  PRINT THE REMOVAL FORECAST, SOONEST FIRST
C     ================================================================
      DO 500 I = 1, NENG
         RKEY(I) = -FLOAT(IRUL(I))
         IF (ISTAT(I) .EQ. KGREY) RKEY(I) = -99999.0
  500 CONTINUE
      CALL SRTIDX (RKEY, INDX, NENG)
C
      NRED = 0
      NAMB = 0
      NGRN = 0
      NGRY = 0
      DO 550 I = 1, NENG
         IF (ISTAT(I) .EQ. KRED) NRED = NRED + 1
         IF (ISTAT(I) .EQ. KAMBER) NAMB = NAMB + 1
         IF (ISTAT(I) .EQ. KGREEN) NGRN = NGRN + 1
         IF (ISTAT(I) .EQ. KGREY) NGRY = NGRY + 1
  550 CONTINUE
C
      CALL PAGHDR (LUPRT, 'REMOVAL FORECAST - STEP 050', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9000)
      DO 600 M = 1, NENG
         I = INDX(M)
         IF (ISTAT(I) .EQ. KGREY) GO TO 600
         IF (IRUL(I) .GT. 2500) GO TO 600
         IF (NLINE .LT. MAXLIN) GO TO 580
         CALL PAGHDR (LUPRT, 'REMOVAL FORECAST - STEP 050', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9000)
  580    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CFAM(I), COPR(I), CTAIL(I),
     +                      CLOCN(I), EGTM(I), ICSO(I), IRUL(I),
     +                      CMODUL(JMOD(I)), JCYC(I), HSCOR(I),
     +                      RISK(I), CSTAT(ISTAT(I))
         NLINE = NLINE + 1
  600 CONTINUE
C
      WRITE (LUPRT,9020) NENG, NPERF, NMODL, NRED, NAMB, NGRN, NGRY
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT (' 0ENGINES FORECAST FOR REMOVAL WITHIN TWO THOUSAND ',
     +        'FIVE HUNDRED CYCLES' /
     +        ' 0SERIAL   FAM  OPR TAIL   STN    EGTM    CSO    RUL',
     +        '  LIMITING MOD  MODLIFE  SCORE   RISK  STATUS' /
     +        ' ', 118('-'))
 9010 FORMAT (' ', A8, 1X, A4, 1X, A2, 2X, A6, 1X, A4, 1X, F7.1,
     +        1X, I6, 1X, I6, 2X, A9, 1X, I8, 2X, F5.1, 1X, F6.4,
     +        2X, A5)
 9020 FORMAT (' 0', 118('-') /
     +        ' ENGINES PROJECTED              ', I8 /
     +        ' LIMITED BY PERFORMANCE         ', I8 /
     +        ' LIMITED BY MODULE LIFE         ', I8 /
     +        ' FLEET ROLL UP  RED             ', I8 /
     +        '                AMBER           ', I8 /
     +        '                GREEN           ', I8 /
     +        '                NO DATA         ', I8 /
     +        ' 0EHM2050I STEP 050 COMPLETE')
      END
