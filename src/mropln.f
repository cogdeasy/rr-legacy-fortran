C=====================================================================
C     MROPLN   STEP 060 - SHOP VISIT SLOT ALLOCATION
C
C     TURNS THE REMOVAL FORECAST INTO A RESOURCED INDUCTION PLAN.
C     ENGINES ARE WORKED IN URGENCY ORDER, SOONEST FORECAST REMOVAL
C     FIRST, AND EACH IS OFFERED THE LATEST SLOT THAT STILL LANDS
C     BEFORE ITS FORECAST REMOVAL WEEK.  TAKING THE LATEST ACCEPTABLE
C     SLOT RATHER THAN THE EARLIEST KEEPS THE ENGINE EARNING ON WING
C     FOR AS LONG AS THE FORECAST ALLOWS, WHICH IS THE STANDING
C     INSTRUCTION FROM THE FLEET MANAGEMENT COMMITTEE.
C
C     THE WORKSCOPE IS DERIVED FROM THE LIMITING MODULE
C
C        HEVY  FULL OVERHAUL - RED ENGINE OR LESS THAN 200 CYCLES
C        HOTS  HOT SECTION REFURBISHMENT - COMBUSTOR, HPT OR IPT
C        PERF  PERFORMANCE RESTORATION - FAN, IPC OR HPC
C        LGHT  LIGHT VISIT - GEARBOX, ACCESSORIES, NACELLE
C
C     A SLOT MAY ONLY BE TAKEN WHERE THE SHOP HOLDS THE CAPABILITY.
C     A SHOP CODED FULL MAY ACCEPT ANY WORKSCOPE.
C
C     FILES     ENGMAS  INPUT   RULMAS  INPUT   SHPSLT  INPUT
C               SVPLAN  OUTPUT  SYSERR  OUTPUT
C=====================================================================
      PROGRAM MROPLN
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      INTEGER CYCWK
      PARAMETER (CYCWK = 21)
C
      INTEGER JMOD(MAXENG), JCYC(MAXENG), INDX(MAXENG)
      REAL RISK(MAXENG), RKEY(MAXENG)
      INTEGER ISWK(MAXSLT), ISCAP(MAXSLT), ISUSE(MAXSLT)
      CHARACTER*4 CSLOT(MAXSLT), CSBAS(MAXSLT), CSCAP(MAXSLT)
C
      INTEGER I, K, M, NSLT, IPAGE, NLINE, IWKNOW, IWKREQ, IWKBST
      INTEGER KBEST, NPLAN, NUNAL, NOVER, IPRI, NWKS, IYR, IDAY
      INTEGER IWKADD, IWKDIF
      CHARACTER*4 CWS
      CHARACTER*9 CMODUL
      CHARACTER*5 CSTAT
      EXTERNAL IWKADD, IWKDIF, CMODUL, CSTAT
C
C     ================================================================
C     STEP 1.  LOAD THE MASTERS AND THE SLOT FILE
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      CALL ENGLOD
      CALL RULLOD (JMOD, JCYC, RISK)
      CALL SLTLOD (NSLT, CSLOT, CSBAS, ISWK, ISCAP, CSCAP)
      IPAGE = 0
      NLINE = 99
      NPLAN = 0
      NUNAL = 0
      NOVER = 0
C
      DO 100 K = 1, NSLT
         ISUSE(K) = 0
  100 CONTINUE
C
C     ---- CURRENT WEEK, FORM YYWW, FROM THE RUN DATE -----------------
      IYR = JRUNDT / 1000
      IDAY = JRUNDT - IYR * 1000
      IWKNOW = IYR * 100 + (IDAY - 1) / 7 + 1
C
C     ================================================================
C     STEP 2.  WORK THE FLEET IN URGENCY ORDER
C     ================================================================
      DO 200 I = 1, NENG
         RKEY(I) = -FLOAT(IRUL(I))
         IF (ISTG(I) .EQ. 4) RKEY(I) = -99999.0
         IF (ISTAT(I) .EQ. KGREY) RKEY(I) = -99999.0
  200 CONTINUE
      CALL SRTIDX (RKEY, INDX, NENG)
C
      CALL OPNSEQ (LUPLN, 'SVPLAN.DAT', 'NEW')
      CALL PAGHDR (LUPRT, 'SHOP VISIT PLAN - STEP 060', IPAGE, NLINE)
      WRITE (LUPRT,9000)
C
      DO 500 M = 1, NENG
         I = INDX(M)
         IF (ISTG(I) .EQ. 4) GO TO 500
         IF (ISTAT(I) .EQ. KGREY) GO TO 500
C
C        ---- ONLY PLAN ENGINES INSIDE THE PLANNING HORIZON -----------
         IF (IRUL(I) .GT. 3000 .AND. ISTAT(I) .EQ. KGREEN) GO TO 500
C
C        ---- WORKSCOPE FROM THE LIMITING MODULE ----------------------
         CWS = 'LGHT'
         IF (JMOD(I) .GE. 1 .AND. JMOD(I) .LE. 3) CWS = 'PERF'
         IF (JMOD(I) .GE. 4 .AND. JMOD(I) .LE. 6) CWS = 'HOTS'
         IF (JMOD(I) .EQ. 7) CWS = 'HOTS'
         IF (IRUL(I) .LT. 200 .OR. ISTAT(I) .EQ. KRED) CWS = 'HEVY'
C
C        ---- FORECAST REMOVAL WEEK -----------------------------------
         NWKS = (IRUL(I) - 150) / CYCWK
         IF (NWKS .LT. 0) NWKS = 0
         IF (NWKS .GT. 156) NWKS = 156
         IWKREQ = IWKADD(IWKNOW, NWKS)
C
C        ---- PRIORITY BAND FOR THE PLANNING BOARD --------------------
         IPRI = 3
         IF (ISTAT(I) .EQ. KAMBER) IPRI = 2
         IF (ISTAT(I) .EQ. KRED) IPRI = 1
C
C        ---- SELECT THE LATEST ACCEPTABLE SLOT -----------------------
         KBEST = 0
         IWKBST = -1
         DO 300 K = 1, NSLT
            IF (ISUSE(K) .GE. ISCAP(K)) GO TO 300
            IF (CSCAP(K) .NE. 'FULL' .AND. CSCAP(K) .NE. CWS) GO TO 300
            IF (IWKDIF(ISWK(K), IWKNOW) .LT. 0) GO TO 300
            IF (IWKDIF(IWKREQ, ISWK(K)) .LT. 0) GO TO 300
            IF (IWKDIF(ISWK(K), IWKBST) .LE. 0 .AND. KBEST .NE. 0)
     +         GO TO 300
            KBEST = K
            IWKBST = ISWK(K)
  300    CONTINUE
C
         IF (KBEST .NE. 0) GO TO 400
C
C        ---- NO SLOT AVAILABLE ---------------------------------------
         NUNAL = NUNAL + 1
         IF (IPRI .EQ. 1) NOVER = NOVER + 1
         CALL PLNWR (LUPLN, CESN(I), '****', '****', 0, CWS, IPRI,
     +               IRUL(I))
         IF (NLINE .LT. MAXLIN) GO TO 350
         CALL PAGHDR (LUPRT, 'SHOP VISIT PLAN - STEP 060', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9000)
  350    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CFAM(I), COPR(I), CSTAT(ISTAT(I)),
     +                      IRUL(I), CMODUL(JMOD(I)), CWS, IPRI,
     +                      '****', '****', IWKREQ, 0
         NLINE = NLINE + 1
         CALL ERRMSG (2061, 'NO SHOP SLOT AVAILABLE BEFORE REMOVAL')
         GO TO 500
C
C        ---- SLOT ALLOCATED ------------------------------------------
  400    CONTINUE
         ISUSE(KBEST) = ISUSE(KBEST) + 1
         NPLAN = NPLAN + 1
         CALL PLNWR (LUPLN, CESN(I), CSLOT(KBEST), CSBAS(KBEST),
     +               ISWK(KBEST), CWS, IPRI, IRUL(I))
         IF (NLINE .LT. MAXLIN) GO TO 450
         CALL PAGHDR (LUPRT, 'SHOP VISIT PLAN - STEP 060', IPAGE,
     +                NLINE)
         WRITE (LUPRT,9000)
  450    CONTINUE
         WRITE (LUPRT,9010) CESN(I), CFAM(I), COPR(I), CSTAT(ISTAT(I)),
     +                      IRUL(I), CMODUL(JMOD(I)), CWS, IPRI,
     +                      CSLOT(KBEST), CSBAS(KBEST), IWKREQ,
     +                      ISWK(KBEST)
         NLINE = NLINE + 1
  500 CONTINUE
      CLOSE (LUPLN)
C
C     ================================================================
C     STEP 3.  SHOP LOADING
C     ================================================================
      CALL PAGHDR (LUPRT, 'SHOP LOADING - STEP 060', IPAGE, NLINE)
      WRITE (LUPRT,9020)
      DO 600 K = 1, NSLT
         WRITE (LUPRT,9030) CSLOT(K), CSBAS(K), ISWK(K), CSCAP(K),
     +                      ISCAP(K), ISUSE(K), ISCAP(K) - ISUSE(K)
  600 CONTINUE
C
      WRITE (LUPRT,9040) NSLT, NPLAN, NUNAL, NOVER
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('0SERIAL   FAM  OPR STATUS    RUL  LIMITING MOD  ',
     +        'SCOPE PRI  SLOT SHOP  REQ WEEK  ALLOC WEEK' /
     +        ' ', 110('-'))
 9010 FORMAT (' ', A8, 1X, A4, 1X, A2, 2X, A5, 1X, I6, 2X, A9,
     +        2X, A4, 3X, I1, 3X, A4, 1X, A4, 6X, I4, 8X, I4)
 9020 FORMAT ('0SLOT SHOP  WEEK CAPABILITY  BAYS  TAKEN  FREE' /
     +        ' ', 48('-'))
 9030 FORMAT (' ', A4, 1X, A4, 1X, I4, 1X, A4, 8X, I4, 3X, I4,
     +        2X, I4)
 9040 FORMAT ('0', 48('-') /
     +        ' SLOTS ON FILE                  ', I8 /
     +        ' ENGINES ALLOCATED              ', I8 /
     +        ' ENGINES NOT ALLOCATED          ', I8 /
     +        ' OF WHICH RED PRIORITY          ', I8 /
     +        '0EHM2060I STEP 060 COMPLETE')
      END
C
      INTEGER FUNCTION IWKADD (IYW, N)
C     ----------------------------------------------------------------
C     ADD N WEEKS TO A PLANNING WEEK OF FORM YYWW.  THE PLANNING YEAR
C     IS TAKEN AS FIFTY TWO WEEKS, AS IT IS ON THE SHOP LOADING BOARD.
C     ----------------------------------------------------------------
      INTEGER IYW, N, IY, IW
C
      IY = IYW / 100
      IW = IYW - IY * 100 + N
  100 CONTINUE
      IF (IW .LE. 52) GO TO 200
      IW = IW - 52
      IY = IY + 1
      IF (IY .GT. 99) IY = 0
      GO TO 100
  200 CONTINUE
      IWKADD = IY * 100 + IW
      RETURN
      END
C
      INTEGER FUNCTION IWKDIF (IYW1, IYW2)
C     ----------------------------------------------------------------
C     WEEKS BETWEEN TWO PLANNING WEEKS, POSITIVE WHEN THE FIRST IS
C     THE LATER.  A NEGATIVE SECOND ARGUMENT MEANS NO WEEK HAS BEEN
C     CHOSEN YET AND THE FIRST IS THEN ALWAYS THE LATER.
C     ----------------------------------------------------------------
      INTEGER IYW1, IYW2, IY1, IY2, IW1, IW2
C
      IF (IYW2 .GE. 0) GO TO 100
      IWKDIF = 1
      RETURN
  100 CONTINUE
      IY1 = IYW1 / 100
      IY2 = IYW2 / 100
      IW1 = IYW1 - IY1 * 100
      IW2 = IYW2 - IY2 * 100
      IWKDIF = (IY1 - IY2) * 52 + (IW1 - IW2)
      RETURN
      END
