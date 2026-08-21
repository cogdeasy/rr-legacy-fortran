C=====================================================================
C     FLTRPT   STEP 070 - FLEET STATUS REPORT
C
C     THE MORNING REPORT.  PRINTS, IN ORDER
C
C        SECTION 1  FLEET ROLL UP BY OPERATOR
C        SECTION 2  ENGINE WATCHLIST, WORST HEALTH SCORE FIRST
C        SECTION 3  ALERT SUMMARY BY SEVERITY AND BY SOURCE
C        SECTION 4  SHOP VISIT PLAN AS ALLOCATED BY STEP 060
C        SECTION 5  RUN STATISTICS AND END OF REPORT
C
C     THE REPORT IS THE FORMAL RECORD OF THE OVERNIGHT RUN.  ONE COPY
C     GOES TO THE DUTY CONTROLLER, ONE TO THE CUSTOMER ACCOUNT DESK
C     AND ONE TO THE MICROFICHE ARCHIVE.
C
C     FILES     ENGMAS  INPUT   TRNMAS  INPUT   ALRTMS  INPUT
C               RULMAS  INPUT   SVPLAN  INPUT   SYSERR  OUTPUT
C=====================================================================
      PROGRAM FLTRPT
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      INTEGER MAXOPR
      PARAMETER (MAXOPR = 20)
C
      INTEGER JMOD(MAXENG), JCYC(MAXENG), INDX(MAXENG)
      REAL RISK(MAXENG), RKEY(MAXENG)
      CHARACTER*2 CONAM(MAXOPR)
      INTEGER NOENG(MAXOPR), NORED(MAXOPR), NOAMB(MAXOPR)
      INTEGER NOGRN(MAXOPR), NOGRY(MAXOPR)
      REAL SOSCOR(MAXOPR)
      INTEGER NSEVT(5), NSRCT(5)
C
      INTEGER I, J, K, M, NOPR, IPAGE, NLINE, IRC, NLIST
      INTEGER NRED, NAMB, NGRN, NGRY, IWK, IPRI, NRUL, NPL
      INTEGER ENGFND
      REAL FLSCOR, AVSCOR
      CHARACTER*8 CE
      CHARACTER*4 CSLOT, CBASE, CWS
      CHARACTER*5 CSTAT
      CHARACTER*8 CSEV, CSRC
      CHARACTER*9 CMODUL
      EXTERNAL ENGFND, CSTAT, CSEV, CSRC, CMODUL
C
C     ================================================================
C     STEP 1.  LOAD EVERY MASTER PRODUCED BY THE JOB STREAM
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      CALL TRNLOD
      CALL RULLOD (JMOD, JCYC, RISK)
      CALL ALRLOD
      IPAGE = 0
      NLINE = 99
C
C     ================================================================
C     SECTION 1.  FLEET ROLL UP BY OPERATOR
C     ================================================================
      NOPR = 0
      NRED = 0
      NAMB = 0
      NGRN = 0
      NGRY = 0
      FLSCOR = 0.0
C
      DO 200 I = 1, NENG
         J = 0
         DO 100 K = 1, NOPR
            IF (CONAM(K) .EQ. COPR(I)) J = K
  100    CONTINUE
         IF (J .NE. 0) GO TO 150
         IF (NOPR .GE. MAXOPR) GO TO 160
         NOPR = NOPR + 1
         J = NOPR
         CONAM(J) = COPR(I)
         NOENG(J) = 0
         NORED(J) = 0
         NOAMB(J) = 0
         NOGRN(J) = 0
         NOGRY(J) = 0
         SOSCOR(J) = 0.0
  150    CONTINUE
         NOENG(J) = NOENG(J) + 1
         SOSCOR(J) = SOSCOR(J) + HSCOR(I)
         IF (ISTAT(I) .EQ. KRED) NORED(J) = NORED(J) + 1
         IF (ISTAT(I) .EQ. KAMBER) NOAMB(J) = NOAMB(J) + 1
         IF (ISTAT(I) .EQ. KGREEN) NOGRN(J) = NOGRN(J) + 1
         IF (ISTAT(I) .EQ. KGREY) NOGRY(J) = NOGRY(J) + 1
  160    CONTINUE
         IF (ISTAT(I) .EQ. KRED) NRED = NRED + 1
         IF (ISTAT(I) .EQ. KAMBER) NAMB = NAMB + 1
         IF (ISTAT(I) .EQ. KGREEN) NGRN = NGRN + 1
         IF (ISTAT(I) .EQ. KGREY) NGRY = NGRY + 1
         FLSCOR = FLSCOR + HSCOR(I)
  200 CONTINUE
C
      CALL PAGHDR (LUPRT, 'SECTION 1 - FLEET ROLL UP', IPAGE, NLINE)
      WRITE (LUPRT,9000)
      DO 250 K = 1, NOPR
         AVSCOR = 0.0
         IF (NOENG(K) .GT. 0) AVSCOR = SOSCOR(K) / FLOAT(NOENG(K))
         WRITE (LUPRT,9010) CONAM(K), NOENG(K), NORED(K), NOAMB(K),
     +                      NOGRN(K), NOGRY(K), AVSCOR
  250 CONTINUE
      AVSCOR = 0.0
      IF (NENG .GT. 0) AVSCOR = FLSCOR / FLOAT(NENG)
      WRITE (LUPRT,9020) NENG, NRED, NAMB, NGRN, NGRY, AVSCOR
C
C     ================================================================
C     SECTION 2.  ENGINE WATCHLIST
C     ================================================================
      DO 300 I = 1, NENG
         RKEY(I) = 100.0 - HSCOR(I)
         IF (ISTAT(I) .EQ. KGREY) RKEY(I) = -1.0
  300 CONTINUE
      CALL SRTIDX (RKEY, INDX, NENG)
C
      CALL PAGHDR (LUPRT, 'SECTION 2 - ENGINE WATCHLIST', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9100)
      NLIST = 0
      DO 350 M = 1, NENG
         I = INDX(M)
         IF (ISTAT(I) .EQ. KGREY) GO TO 350
         IF (NLIST .GE. 40) GO TO 360
         IF (ISTAT(I) .EQ. KGREEN .AND. NLIST .GE. 20) GO TO 360
         IF (NLINE .LT. MAXLIN) GO TO 320
         CALL PAGHDR (LUPRT, 'SECTION 2 - ENGINE WATCHLIST', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9100)
  320    CONTINUE
         WRITE (LUPRT,9110) CESN(I), CFAM(I), COPR(I), CTAIL(I),
     +                      CLOCN(I), IENVS(I), EGTM(I), TSLOPE(I),
     +                      TVBMX(I), TOILC(I), TDEBR(I), IRUL(I),
     +                      CMODUL(JMOD(I)), HSCOR(I),
     +                      CSTAT(ISTAT(I))
         NLINE = NLINE + 1
         NLIST = NLIST + 1
  350 CONTINUE
  360 CONTINUE
C
C     ================================================================
C     SECTION 3.  ALERT SUMMARY
C     ================================================================
      DO 400 K = 1, 5
         NSEVT(K) = 0
         NSRCT(K) = 0
  400 CONTINUE
      DO 420 K = 1, NALR
         IF (IASEV(K) .GE. 1 .AND. IASEV(K) .LE. 5)
     +      NSEVT(IASEV(K)) = NSEVT(IASEV(K)) + 1
         IF (IASRC(K) .GE. 1 .AND. IASRC(K) .LE. 5)
     +      NSRCT(IASRC(K)) = NSRCT(IASRC(K)) + 1
  420 CONTINUE
C
      CALL PAGHDR (LUPRT, 'SECTION 3 - ALERT SUMMARY', IPAGE, NLINE)
      WRITE (LUPRT,9200)
      DO 440 K = 1, 5
         WRITE (LUPRT,9210) CSEV(K), NSEVT(K), CSRC(K), NSRCT(K)
  440 CONTINUE
      WRITE (LUPRT,9220) NALR
C
C     ---- CRITICAL AND HIGH ALERTS IN FULL ---------------------------
      WRITE (LUPRT,9230)
      NLINE = NLINE + 10
      DO 460 K = 1, NALR
         IF (IASEV(K) .GT. 2) GO TO 460
         I = IAENG(K)
         IF (NLINE .LT. MAXLIN) GO TO 450
         CALL PAGHDR (LUPRT, 'SECTION 3 - ALERT SUMMARY', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9230)
  450    CONTINUE
         WRITE (LUPRT,9240) CESN(I), CTAIL(I), CLOCN(I),
     +                      CSEV(IASEV(K)), CSRC(IASRC(K)), CAPRM(K),
     +                      AVAL(K), ALIM(K), CATXT(K)
         NLINE = NLINE + 1
  460 CONTINUE
C
C     ================================================================
C     SECTION 4.  SHOP VISIT PLAN
C     ================================================================
      CALL PAGHDR (LUPRT, 'SECTION 4 - SHOP VISIT PLAN', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9300)
      CALL OPNSEQ (LUPLN, 'SVPLAN.DAT', 'OLD')
      NPL = 0
  500 CONTINUE
      CALL PLNRD (LUPLN, CE, CSLOT, CBASE, IWK, CWS, IPRI, NRUL, IRC)
      IF (IRC .EQ. KEOF) GO TO 560
      IF (IRC .EQ. KBAD) GO TO 550
      I = ENGFND(CE)
      IF (I .EQ. 0) GO TO 500
      NPL = NPL + 1
      IF (NLINE .LT. MAXLIN) GO TO 520
      CALL PAGHDR (LUPRT, 'SECTION 4 - SHOP VISIT PLAN', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9300)
  520 CONTINUE
      WRITE (LUPRT,9310) CE, CFAM(I), COPR(I), CTAIL(I), IPRI, NRUL,
     +                   CWS, CSLOT, CBASE, IWK, CSTAT(ISTAT(I))
      NLINE = NLINE + 1
      GO TO 500
  550 CONTINUE
      CALL ERRMSG (2071, 'PLAN RECORD REJECTED - BAD DATA')
      GO TO 500
  560 CONTINUE
      CLOSE (LUPLN)
      WRITE (LUPRT,9320) NPL
C
C     ================================================================
C     SECTION 5.  RUN STATISTICS
C     ================================================================
      CALL PAGHDR (LUPRT, 'SECTION 5 - RUN STATISTICS', IPAGE, NLINE)
      WRITE (LUPRT,9400) CRUNID, JRUNDT, JWINDW, NENG, NALR, NPL,
     +                   NRED, NAMB, NGRN, NGRY, AVSCOR
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('0OPR  ENGINES    RED  AMBER  GREEN  NODATA  ',
     +        'MEAN SCORE' /
     +        ' ', 52('-'))
 9010 FORMAT (' ', A2, 3X, I5, 3X, I5, 2X, I5, 2X, I5, 3X, I5,
     +        4X, F8.1)
 9020 FORMAT (' ', 52('-') /
     +        ' ALL', 2X, I5, 3X, I5, 2X, I5, 2X, I5, 3X, I5,
     +        4X, F8.1)
 9100 FORMAT ('0SERIAL   FAM  OPR TAIL   STN  ENV    EGTM',
     +        '   SLOPE/CYC  VIBMAX  OILC/H  DEBRIS     RUL',
     +        '  LIMITING MOD  SCORE  STATUS' /
     +        ' ', 128('-'))
 9110 FORMAT (' ', A8, 1X, A4, 1X, A2, 2X, A6, 1X, A4, 3X, I1,
     +        1X, F7.1, 2X, F9.5, 2X, F6.2, 2X, F6.3, 2X, F6.1,
     +        1X, I7, 2X, A9, 2X, F5.1, 2X, A5)
 9200 FORMAT ('0SEVERITY   COUNT    SOURCE     COUNT' /
     +        ' ', 40('-'))
 9210 FORMAT (' ', A8, 1X, I7, 4X, A8, 2X, I7)
 9220 FORMAT (' ', 40('-') /
     +        ' TOTAL ALERTS RAISED ', I7)
 9230 FORMAT ('0CRITICAL AND HIGH SEVERITY ALERTS' /
     +        '0SERIAL   TAIL   STN  SEVERITY SOURCE   PARM',
     +        '   OBSERVED     LIMIT  NARRATIVE' /
     +        ' ', 118('-'))
 9240 FORMAT (' ', A8, 1X, A6, 1X, A4, 1X, A8, 1X, A8, 1X, A4, 1X,
     +        F10.3, 1X, F9.3, 2X, A44)
 9300 FORMAT ('0SERIAL   FAM  OPR TAIL   PRI     RUL  SCOPE  SLOT',
     +        ' SHOP  WEEK  STATUS' /
     +        ' ', 74('-'))
 9310 FORMAT (' ', A8, 1X, A4, 1X, A2, 2X, A6, 2X, I1, 1X, I7,
     +        2X, A4, 3X, A4, 1X, A4, 1X, I5, 2X, A5)
 9320 FORMAT (' ', 74('-') /
     +        ' ENGINES ON THE PLAN ', I7)
 9400 FORMAT ('0RUN IDENTIFIER                 ', 5X, A8 /
     +        ' RUN DATE, JULIAN               ', I13 /
     +        ' TREND WINDOW, CYCLES           ', I13 /
     +        ' ENGINES ON MASTER              ', I13 /
     +        ' ALERTS RAISED                  ', I13 /
     +        ' ENGINES ON THE SHOP PLAN       ', I13 /
     +        ' FLEET ROLL UP  RED             ', I13 /
     +        '                AMBER           ', I13 /
     +        '                GREEN           ', I13 /
     +        '                NO DATA         ', I13 /
     +        ' MEAN FLEET HEALTH SCORE        ', F13.1 /
     +        '0EHM2070I STEP 070 COMPLETE - END OF REPORT')
      END
