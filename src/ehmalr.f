C=====================================================================
C     EHMALR   STEP 040 - EXCEEDANCE DETECTION AND ALERT RAISING
C
C     COMPARES THE LATEST DOWNLINKED OBSERVATION AND THE TREND WINDOW
C     FOR EACH ENGINE AGAINST THE OPERATOR AND OEM LIMITS HELD ON THE
C     PARAMETER LIMIT FILE, AND RAISES AN ALERT FOR EVERY EXCEEDANCE.
C     THE PRINTED QUEUE IS THE CONTROLLER WORK LIST FOR THE SHIFT:
C     IT IS ORDERED BY SEVERITY, THEN BY THE SIZE OF THE BREACH.
C
C     SEVERITY IS ALLOCATED AS FOLLOWS
C
C        1 CRITICAL  RED ON EGT MARGIN OR ON FAN SHAFT VIBRATION,
C                    THE TWO PARAMETERS THAT CAN FORCE A SECTOR TO
C                    BE REFUSED
C        2 HIGH      ANY OTHER RED EXCEEDANCE, OR A STEEP AND WELL
C                    FITTED DETERIORATION TREND
C        3 MEDIUM    ANY AMBER EXCEEDANCE
C
C     FILES     ENGMAS  INPUT   FLTVAL  INPUT   PARLIM  INPUT
C               TRNMAS  INPUT   ALRTMS  OUTPUT  SYSERR  OUTPUT
C=====================================================================
      PROGRAM EHMALR
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      REAL VLAST(NFVAL,MAXENG)
      INTEGER NSEEN(MAXENG), JLAST(MAXENG), INDX(MAXALR)
      REAL RANK(MAXALR)
C
      INTEGER I, J, K, IRC, JD, IPAGE, NLINE, NCRIT, NHIGH, NMED
      INTEGER ENGFND
      REAL V(NFVAL)
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
      CHARACTER*8 CSEV, CSRC
      EXTERNAL ENGFND, CSEV, CSRC
C
C     ================================================================
C     STEP 1.  INITIALISE AND LOAD THE MASTERS
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      CALL LIMLOD
      CALL TRNLOD
      IPAGE = 0
      NLINE = 99
      NALR = 0
C
      DO 110 I = 1, NENG
         NSEEN(I) = 0
         JLAST(I) = 0
         DO 100 K = 1, NFVAL
            VLAST(K,I) = 0.0
  100    CONTINUE
  110 CONTINUE
C
C     ================================================================
C     STEP 2.  RETAIN THE LATEST OBSERVATION FOR EACH ENGINE
C     ================================================================
      CALL OPNSEQ (LUVAL, 'FLTVAL.DAT', 'OLD')
  200 CONTINUE
      CALL FLTRD (LUVAL, CE, JD, CFLT, CORG, CDST, V, IRC)
      IF (IRC .EQ. KEOF) GO TO 300
      IF (IRC .EQ. KBAD) GO TO 200
      J = ENGFND(CE)
      IF (J .EQ. 0) GO TO 200
      NSEEN(J) = NSEEN(J) + 1
      IF (JD .LT. JLAST(J)) GO TO 200
      JLAST(J) = JD
      DO 250 K = 1, NFVAL
         VLAST(K,J) = V(K)
  250 CONTINUE
      GO TO 200
  300 CONTINUE
      CLOSE (LUVAL)
C
C     ================================================================
C     STEP 3.  TEST EVERY ENGINE AGAINST THE LIMIT FILE
C     ================================================================
      DO 400 I = 1, NENG
         IF (NSEEN(I) .EQ. 0) GO TO 380
C
C        ---- GAS PATH -------------------------------------------------
         CALL ALRTST (I, 'EGTM', VLAST(5,I), JLAST(I), 1,
     +                'EGT MARGIN AT OR BELOW ALERT LIMIT          ')
         CALL ALRTST (I, 'TIPC', VLAST(17,I), JLAST(I), 1,
     +                'HPT TIP CLEARANCE OPENED BEYOND LIMIT       ')
C
C        ---- MECHANICAL ----------------------------------------------
         CALL ALRTST (I, 'VIB1', VLAST(9,I), JLAST(I), 3,
     +                'FAN SHAFT BROADBAND VIBRATION EXCEEDANCE    ')
         CALL ALRTST (I, 'VIB2', VLAST(10,I), JLAST(I), 3,
     +                'IP SHAFT BROADBAND VIBRATION EXCEEDANCE     ')
C
C        ---- OIL SYSTEM ----------------------------------------------
         CALL ALRTST (I, 'OILP', VLAST(12,I), JLAST(I), 1,
     +                'OIL PRESSURE BELOW ALERT LIMIT              ')
         CALL ALRTST (I, 'OILC', TOILC(I), JLAST(I), 4,
     +                'OIL CONSUMPTION RATE ABOVE ALERT LIMIT      ')
         CALL ALRTST (I, 'DEBR', TDEBR(I), JLAST(I), 4,
     +                'MAGNETIC DEBRIS COUNT ABOVE ALERT LIMIT     ')
C
C        ---- FUEL BURN -----------------------------------------------
         CALL ALRTST (I, 'SFCD', TSFCD(I), JLAST(I), 5,
     +                'FUEL BURN DEVIATION ABOVE ALERT LIMIT       ')
C
C        ---- TREND DERIVED ALERT --------------------------------------
C        A STEEP SLOPE IS ONLY ACTIONABLE WHEN THE FIT IS SOUND AND
C        THE WINDOW IS LONG ENOUGH TO BE BELIEVED.
         IF (NTOBS(I) .LT. 20) GO TO 380
         IF (TRSQ(I) .LT. 0.30) GO TO 380
         IF (TSLOPE(I) .GT. -0.0120) GO TO 380
         CALL ALRADD (I, 2, 5, 'TRND', TSLOPE(I), -0.0120,
     +                JLAST(I),
     +                'DETERIORATION RATE ABOVE FLEET EXPERIENCE   ')
  380    CONTINUE
         IF (NSEEN(I) .GT. 0) GO TO 400
         CALL ALRADD (I, 4, 1, 'COVR', 0.0, 1.0, JRUNDT,
     +                'NO DOWNLINK RECEIVED IN THE WINDOW          ')
  400 CONTINUE
C
C     ================================================================
C     STEP 4.  WRITE THE ALERT MASTER AND PRINT THE TRIAGE QUEUE
C     ================================================================
      CALL OPNSEQ (LUALR, 'ALRTMS.DAT', 'NEW')
      DO 500 K = 1, NALR
         CALL ALRWR (LUALR, K)
  500 CONTINUE
      CLOSE (LUALR)
C
C     ---- RANK THE QUEUE ---------------------------------------------
      DO 550 K = 1, NALR
         RANK(K) = FLOAT(6 - IASEV(K)) * 1000.0
         IF (ABS(ALIM(K)) .GT. 1.0E-4)
     +      RANK(K) = RANK(K) + 100.0 * ABS((AVAL(K) - ALIM(K))
     +                / ALIM(K))
  550 CONTINUE
      CALL SRTIDX (RANK, INDX, NALR)
C
      NCRIT = 0
      NHIGH = 0
      NMED = 0
      DO 560 K = 1, NALR
         IF (IASEV(K) .EQ. 1) NCRIT = NCRIT + 1
         IF (IASEV(K) .EQ. 2) NHIGH = NHIGH + 1
         IF (IASEV(K) .EQ. 3) NMED = NMED + 1
  560 CONTINUE
C
      CALL PAGHDR (LUPRT, 'ALERT TRIAGE QUEUE - STEP 040', IPAGE,
     +             NLINE)
      WRITE (LUPRT,9000)
      DO 600 K = 1, NALR
         J = INDX(K)
         I = IAENG(J)
         IF (NLINE .LT. MAXLIN) GO TO 580
         CALL PAGHDR (LUPRT, 'ALERT TRIAGE QUEUE - STEP 040', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9000)
  580    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CTAIL(I), CLOCN(I), CSEV(IASEV(J)),
     +                      CSRC(IASRC(J)), CAPRM(J), AVAL(J),
     +                      ALIM(J), IADAT(J), CATXT(J)
         NLINE = NLINE + 1
  600 CONTINUE
C
      WRITE (LUPRT,9020) NALR, NCRIT, NHIGH, NMED
      IF (NALR .GE. MAXALR) CALL ERRMSG (2041,
     +   'ALERT TABLE FULL - QUEUE MAY BE INCOMPLETE')
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('0SERIAL   TAIL   STN  SEVERITY SOURCE   PARM',
     +        '   OBSERVED     LIMIT   DATE  NARRATIVE' /
     +        ' ', 124('-'))
 9010 FORMAT (' ', A8, 1X, A6, 1X, A4, 1X, A8, 1X, A8, 1X, A4, 1X,
     +        F10.3, 1X, F9.3, 1X, I5, 2X, A44)
 9020 FORMAT ('0', 124('-') /
     +        ' ALERTS RAISED                  ', I8 /
     +        ' SEVERITY CRITICAL              ', I8 /
     +        ' SEVERITY HIGH                  ', I8 /
     +        ' SEVERITY MEDIUM                ', I8 /
     +        '0EHM2040I STEP 040 COMPLETE')
      END
C
      SUBROUTINE ALRTST (I, CKEY, VALUE, JDATE, ISRC, TEXT)
C     ----------------------------------------------------------------
C     TEST ONE OBSERVATION AGAINST ITS LIMIT CARD AND RAISE AN ALERT
C     IF THE VALUE IS AMBER OR RED.  WHEN NO LIMIT CARD EXISTS FOR
C     THE PARAMETER THE TEST IS PASSED OVER IN SILENCE - THIS IS HOW
C     A PARAMETER IS SUPPRESSED FOR A CUSTOMER WHO HAS NOT BOUGHT THE
C     CORRESPONDING SERVICE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER I, JDATE, ISRC
      REAL VALUE
      CHARACTER*(*) CKEY, TEXT
      INTEGER L, IS, ISEV, LIMFND, ISTATC
      REAL RLIM
      EXTERNAL LIMFND, ISTATC
C
      L = LIMFND(CKEY)
      IF (L .EQ. 0) RETURN
      IS = ISTATC(VALUE, PAMB(L), PRED(L), IPSNS(L))
      IF (IS .EQ. KGREEN) RETURN
C
      RLIM = PAMB(L)
      ISEV = 3
      IF (IS .NE. KRED) GO TO 100
      RLIM = PRED(L)
      ISEV = 2
      IF (CKEY .EQ. 'EGTM' .OR. CKEY .EQ. 'VIB1') ISEV = 1
  100 CONTINUE
      CALL ALRADD (I, ISEV, ISRC, CKEY, VALUE, RLIM, JDATE, TEXT)
      RETURN
      END
C
      SUBROUTINE ALRADD (I, ISEV, ISRC, CKEY, VALUE, RLIM, JDATE,
     +                   TEXT)
C     ----------------------------------------------------------------
C     ADD ONE ENTRY TO THE ALERT TABLE.  THE TABLE IS FIXED IN SIZE;
C     ONCE IT IS FULL FURTHER ALERTS ARE COUNTED ON THE ERROR FILE
C     ONLY, AND THE OPERATOR MUST RE-RUN WITH A LARGER TABLE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER I, ISEV, ISRC, JDATE
      REAL VALUE, RLIM
      CHARACTER*(*) CKEY, TEXT
      INTEGER K
C
      IF (NALR .LT. MAXALR) GO TO 100
      CALL ERRMSG (2042, 'ALERT TABLE OVERFLOW - ALERT DISCARDED')
      RETURN
  100 CONTINUE
      K = NALR + 1
      IAENG(K) = I
      IASEV(K) = ISEV
      IASRC(K) = ISRC
      IADAT(K) = JDATE
      CAPRM(K) = CKEY
      AVAL(K) = VALUE
      ALIM(K) = RLIM
      CATXT(K) = TEXT
      NALR = K
      RETURN
      END
