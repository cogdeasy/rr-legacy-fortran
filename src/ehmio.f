C=====================================================================
C     EHMIO    FILE HANDLING MODULE FOR THE EHMS SUITE
C              EVERY RECORD LAYOUT IN THE SYSTEM IS DECLARED ONCE IN
C              THIS DECK.  NO OTHER PROGRAM MAY CODE A READ OR WRITE
C              AGAINST A MASTER FILE.  SEE THE FILE LAYOUT MANUAL,
C              DOCUMENT EHM/FL/004, FOR THE COLUMN CHARTS.
C
C     CONTENTS
C        RDCTL    READ THE RUN CONTROL CARD FROM SYSIN
C        OPNSEQ   OPEN A SEQUENTIAL FILE WITH STATUS CHECKING
C        ENGWR    WRITE ONE ENGINE MASTER RECORD
C        ENGLOD   LOAD THE WHOLE ENGINE MASTER INTO CORE
C        FLTWR    WRITE ONE FLIGHT DOWNLINK RECORD
C        FLTRD    READ ONE FLIGHT DOWNLINK RECORD
C        LIMLOD   LOAD THE PARAMETER LIMIT FILE INTO CORE
C        TRNWR    WRITE ONE TREND MASTER RECORD
C        TRNLOD   LOAD THE TREND MASTER INTO CORE
C        ALRWR    WRITE ONE ALERT MASTER RECORD
C        ALRLOD   LOAD THE ALERT MASTER INTO CORE
C        RULWR    WRITE ONE LIFE PROJECTION RECORD
C        RULLOD   LOAD THE LIFE PROJECTION FILE INTO CORE
C=====================================================================
C
      SUBROUTINE RDCTL
C     ----------------------------------------------------------------
C     READ THE RUN CONTROL CARD.  ONE CARD ONLY, PUNCHED
C
C        COL 01-08  RUN IDENTIFIER
C        COL 10-14  RUN DATE, JULIAN, FORM YYDDD
C        COL 16-20  TREND WINDOW IN FLIGHT CYCLES
C        COL 22-31  GENERATOR SEED
C
C     WHEN THE CARD IS MISSING THE INSTALLATION DEFAULTS ARE TAKEN
C     AND A WARNING IS CUT TO THE ERROR FILE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      CRUNID = 'EHMS0000'
      JRUNDT = 26232
      JWINDW = 400
      JSEED = 20260819
C
      READ (LUCRD,9000,END=800,ERR=800) CRUNID, JRUNDT, JWINDW, JSEED
      IF (JWINDW .LT. 50) JWINDW = 50
      IF (JSEED .LE. 0) JSEED = 20260819
      GO TO 900
C
  800 CONTINUE
      CALL ERRMSG (1001, 'CONTROL CARD ABSENT - DEFAULTS ASSUMED')
  900 CONTINUE
      RETURN
 9000 FORMAT (A8, 1X, I5, 1X, I5, 1X, I10)
      END
C
      SUBROUTINE OPNSEQ (LU, FNAME, CMODE)
C     ----------------------------------------------------------------
C     OPEN A SEQUENTIAL DATA SET.  CMODE IS 'OLD' FOR AN INPUT FILE
C     AND 'NEW' FOR AN OUTPUT FILE.  AN OUTPUT DATA SET IS SCRATCHED
C     AND REALLOCATED SO THAT A RESTART NEVER APPENDS TO THE PRINT OF
C     AN EARLIER RUN.  THE ACCESS INTENT IS DECLARED ON THE OPEN SO
C     THAT A PROTECTED DATA SET IS DIAGNOSED HERE AND NOT LATER ON
C     THE FIRST WRITE.  AN OPEN FAILURE IS FATAL - THE JOB IS
C     ABANDONED WITH A CONDITION CODE OF 16 SO THAT THE FOLLOWING
C     STEPS ARE FLUSHED.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER LU, IOS
      CHARACTER*(*) FNAME, CMODE
C
      IF (CMODE .EQ. 'OLD') GO TO 100
      OPEN (UNIT=LU, FILE=FNAME, STATUS='REPLACE', ACTION='WRITE',
     +      FORM='FORMATTED', IOSTAT=IOS)
      GO TO 200
  100 CONTINUE
      OPEN (UNIT=LU, FILE=FNAME, STATUS='OLD', ACTION='READ',
     +      FORM='FORMATTED', IOSTAT=IOS)
  200 CONTINUE
      IF (IOS .EQ. 0) GO TO 900
      WRITE (LUPRT,9000) LU, FNAME, IOS
      CALL ERRMSG (1002, 'OPEN FAILURE - JOB ABANDONED')
      STOP 16
  900 CONTINUE
      REWIND LU
      RETURN
 9000 FORMAT ('0EHM1002E OPEN FAILED ON UNIT ', I2, ' DSN ', A,
     +        ' IOSTAT ', I6)
      END
C
      SUBROUTINE ENGWR (LU, I)
C     ----------------------------------------------------------------
C     WRITE ENGINE MASTER ENTRY I.  RECORD LENGTH 85 CHARACTERS.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER LU, I
C
      WRITE (LU,9000) CESN(I), CFAM(I), COPR(I), CTAIL(I), IPOSN(I),
     +                CBLD(I), ISTG(I), TFH(I), ITFC(I), HSO(I),
     +                ICSO(I), EGTM(I), IENVS(I), CTHR(I), CLOCN(I)
      RETURN
 9000 FORMAT (A8, 1X, A4, 1X, A2, 1X, A6, 1X, I1, 1X, A6, 1X, I1,
     +        1X, F8.1, 1X, I6, 1X, F8.1, 1X, I6, 1X, F6.1, 1X, I1,
     +        1X, A4, 1X, A4)
      END
C
      SUBROUTINE ENGLOD
C     ----------------------------------------------------------------
C     LOAD THE ENGINE MASTER FILE INTO THE CORE RESIDENT TABLE.  THE
C     FILE IS IN ASCENDING SERIAL NUMBER SEQUENCE; A SEQUENCE CHECK
C     IS APPLIED BECAUSE THE BINARY CHOP IN ENGFND DEPENDS ON IT.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER I
      CHARACTER*8 CLAST
      CHARACTER*160 CBUF
C
      NENG = 0
      CLAST = '        '
      CALL OPNSEQ (LUENG, 'ENGMAS.DAT', 'OLD')
  100 CONTINUE
      I = NENG + 1
      IF (I .GT. MAXENG) GO TO 750
      READ (LUENG,9000,END=700,ERR=810)
     +      CESN(I), CFAM(I), COPR(I), CTAIL(I), IPOSN(I),
     +      CBLD(I), ISTG(I), TFH(I), ITFC(I), HSO(I),
     +      ICSO(I), EGTM(I), IENVS(I), CTHR(I), CLOCN(I)
      IF (CESN(I) .GE. CLAST) GO TO 150
      CALL ERRMSG (1010, 'ENGINE MASTER OUT OF SEQUENCE')
  150 CONTINUE
      CLAST = CESN(I)
      HSCOR(I) = 0.0
      IRUL(I) = 0
      ISTAT(I) = KGREY
      NENG = I
      GO TO 100
C
C     ---- THE TABLE IS FULL.  A FILE HOLDING EXACTLY MAXENG RECORDS
C     ---- IS NOT AN OVERFLOW, SO THE NEXT RECORD IS READ INTO A WORK
C     ---- AREA AND ONLY A GENUINE SURPLUS IS DIAGNOSED.
  750 CONTINUE
      READ (LUENG,9010,END=700,ERR=800) CBUF
      GO TO 800
C
  700 CONTINUE
      CLOSE (LUENG)
      RETURN
  800 CONTINUE
      CALL ERRMSG (1011, 'ENGINE MASTER TABLE OVERFLOW - TRUNCATED')
      CLOSE (LUENG)
      RETURN
  810 CONTINUE
      CALL ERRMSG (1012, 'ENGINE MASTER RECORD REJECTED - BAD DATA')
      GO TO 100
 9000 FORMAT (A8, 1X, A4, 1X, A2, 1X, A6, 1X, I1, 1X, A6, 1X, I1,
     +        1X, F8.1, 1X, I6, 1X, F8.1, 1X, I6, 1X, F6.1, 1X, I1,
     +        1X, A4, 1X, A4)
 9010 FORMAT (A)
      END
C
      SUBROUTINE FLTWR (LU, CE, JDATE, CFLT, CORG, CDST, V)
C     ----------------------------------------------------------------
C     WRITE ONE FLIGHT DOWNLINK RECORD.  THE NUMERIC FIELDS ARE
C     CARRIED IN THE VECTOR V IN THE ORDER
C
C        1 BLOCK HOURS        2 DERATE PCT       3 OAT DEG C
C        4 EGT DEG C          5 EGT MARGIN       6 N1 PCT
C        7 N2 PCT             8 N3 PCT           9 VIB N1
C       10 VIB N2            11 VIB N3          12 OIL PRESS
C       13 OIL TEMP          14 OIL CONSUMPTION 15 FUEL BURN KG
C       16 DEBRIS COUNT      17 TIP CLEARANCE   18 DUST EXPOSURE
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER LU, JDATE
      REAL V(NFVAL)
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
C
      WRITE (LU,9000) CE, JDATE, CFLT, CORG, CDST,
     +                V(1), V(2), V(3), V(4), V(5), V(6), V(7),
     +                V(8), V(9), V(10), V(11), V(12), V(13),
     +                V(14), V(15), NINT(V(16)), V(17), V(18)
      RETURN
 9000 FORMAT (A8, 1X, I5, 1X, A6, 1X, A4, 1X, A4, 1X, F5.2, 1X,
     +        F4.1, 1X, F5.1, 1X, F6.1, 1X, F6.1, 1X, F5.1, 1X,
     +        F5.1, 1X, F5.1, 1X, F4.2, 1X, F4.2, 1X, F4.2, 1X,
     +        F5.1, 1X, F5.1, 1X, F5.2, 1X, F7.0, 1X, I3, 1X,
     +        F5.3, 1X, F4.2)
      END
C
      SUBROUTINE FLTRD (LU, CE, JDATE, CFLT, CORG, CDST, V, IRC)
C     ----------------------------------------------------------------
C     READ ONE FLIGHT DOWNLINK RECORD.  IRC IS RETURNED AS KOK, KEOF
C     OR KBAD.  A BAD RECORD IS COUNTED BY THE CALLER AND THE READ
C     CONTINUES - A CORRUPT DOWNLINK MUST NEVER STOP THE NIGHTLY RUN.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER LU, JDATE, IRC, NDEB
      REAL V(NFVAL)
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
C
      IRC = KOK
      READ (LU,9000,END=800,ERR=810) CE, JDATE, CFLT, CORG, CDST,
     +      V(1), V(2), V(3), V(4), V(5), V(6), V(7), V(8), V(9),
     +      V(10), V(11), V(12), V(13), V(14), V(15), NDEB,
     +      V(17), V(18)
      V(16) = FLOAT(NDEB)
      RETURN
  800 CONTINUE
      IRC = KEOF
      RETURN
  810 CONTINUE
      IRC = KBAD
      RETURN
 9000 FORMAT (A8, 1X, I5, 1X, A6, 1X, A4, 1X, A4, 1X, F5.2, 1X,
     +        F4.1, 1X, F5.1, 1X, F6.1, 1X, F6.1, 1X, F5.1, 1X,
     +        F5.1, 1X, F5.1, 1X, F4.2, 1X, F4.2, 1X, F4.2, 1X,
     +        F5.1, 1X, F5.1, 1X, F5.2, 1X, F7.0, 1X, I3, 1X,
     +        F5.3, 1X, F4.2)
      END
C
      SUBROUTINE LIMLOD
C     ----------------------------------------------------------------
C     LOAD THE PARAMETER LIMIT FILE.  CARDS BEGINNING WITH AN
C     ASTERISK IN COLUMN ONE ARE COMMENTARY AND ARE PASSED OVER.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER I
      CHARACTER*80 CCARD
C
      NPRM = 0
      CALL OPNSEQ (LULIM, 'PARLIM.DAT', 'OLD')
  100 CONTINUE
      READ (LULIM,9000,END=700) CCARD
      IF (CCARD(1:1) .EQ. '*') GO TO 100
      IF (CCARD(1:4) .EQ. '    ') GO TO 100
      I = NPRM + 1
      IF (I .GT. MAXPRM) GO TO 800
      READ (CCARD,9010,ERR=810) CPRM(I), CPUNI(I), IPSNS(I),
     +                          PAMB(I), PRED(I), CPATA(I)
      NPRM = I
      GO TO 100
C
  700 CONTINUE
      CLOSE (LULIM)
      IF (NPRM .EQ. 0) CALL ERRMSG (1020, 'LIMIT FILE IS EMPTY')
      RETURN
  800 CONTINUE
      CALL ERRMSG (1021, 'LIMIT TABLE OVERFLOW - CARDS IGNORED')
      CLOSE (LULIM)
      RETURN
  810 CONTINUE
      CALL ERRMSG (1022, 'LIMIT CARD REJECTED - BAD DATA')
      GO TO 100
 9000 FORMAT (A80)
 9010 FORMAT (A4, 1X, A4, 1X, I1, 1X, F8.2, 1X, F8.2, 1X, A6)
      END
C
      SUBROUTINE TRNWR (LU, I)
C     ----------------------------------------------------------------
C     WRITE ONE TREND MASTER RECORD FOR ENGINE TABLE ENTRY I.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER LU, I
C
      WRITE (LU,9000) CESN(I), NTOBS(I), TSLOPE(I), TINTC(I),
     +                TRSQ(I), TVBMX(I), TOILC(I), TDEBR(I),
     +                TSFCD(I)
      RETURN
 9000 FORMAT (A8, 1X, I5, 1X, F10.6, 1X, F8.2, 1X, F5.3, 1X,
     +        F5.2, 1X, F6.3, 1X, F6.1, 1X, F6.2)
      END
C
      SUBROUTINE TRNLOD
C     ----------------------------------------------------------------
C     LOAD THE TREND MASTER INTO CORE AGAINST THE ENGINE TABLE.  THE
C     ENGINE MASTER MUST ALREADY BE LOADED.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER I, J, N, ENGFND
      REAL SLOPE, YINT, RSQ, VBMX, OILC, DEBR, SFCD
      CHARACTER*8 CE
      EXTERNAL ENGFND
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
      CALL OPNSEQ (LUTRN, 'TRNMAS.DAT', 'OLD')
  200 CONTINUE
      READ (LUTRN,9000,END=700,ERR=800) CE, N, SLOPE, YINT, RSQ,
     +                                  VBMX, OILC, DEBR, SFCD
      J = ENGFND(CE)
      IF (J .EQ. 0) GO TO 810
      NTOBS(J) = N
      TSLOPE(J) = SLOPE
      TINTC(J) = YINT
      TRSQ(J) = RSQ
      TVBMX(J) = VBMX
      TOILC(J) = OILC
      TDEBR(J) = DEBR
      TSFCD(J) = SFCD
      GO TO 200
C
  700 CONTINUE
      CLOSE (LUTRN)
      RETURN
  800 CONTINUE
      CALL ERRMSG (1030, 'TREND RECORD REJECTED - BAD DATA')
      GO TO 200
  810 CONTINUE
      CALL ERRMSG (1031, 'TREND RECORD FOR UNKNOWN SERIAL')
      GO TO 200
 9000 FORMAT (A8, 1X, I5, 1X, F10.6, 1X, F8.2, 1X, F5.3, 1X,
     +        F5.2, 1X, F6.3, 1X, F6.1, 1X, F6.2)
      END
C
      SUBROUTINE ALRWR (LU, K)
C     ----------------------------------------------------------------
C     WRITE ONE ALERT MASTER RECORD FROM ALERT TABLE ENTRY K.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER LU, K
C
      WRITE (LU,9000) CESN(IAENG(K)), IADAT(K), IASEV(K), IASRC(K),
     +                CAPRM(K), AVAL(K), ALIM(K), CATXT(K)
      RETURN
 9000 FORMAT (A8, 1X, I5, 1X, I1, 1X, I1, 1X, A4, 1X, F9.3, 1X,
     +        F9.3, 1X, A44)
      END
C
      SUBROUTINE ALRLOD
C     ----------------------------------------------------------------
C     LOAD THE ALERT MASTER INTO THE CORE RESIDENT ALERT TABLE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER K, J, ENGFND
      CHARACTER*8 CE
      CHARACTER*160 CBUF
      EXTERNAL ENGFND
C
      NALR = 0
      CALL OPNSEQ (LUALR, 'ALRTMS.DAT', 'OLD')
  100 CONTINUE
      K = NALR + 1
      IF (K .GT. MAXALR) GO TO 750
      READ (LUALR,9000,END=700,ERR=810) CE, IADAT(K), IASEV(K),
     +      IASRC(K), CAPRM(K), AVAL(K), ALIM(K), CATXT(K)
      J = ENGFND(CE)
      IF (J .EQ. 0) GO TO 820
      IAENG(K) = J
      NALR = K
      GO TO 100
C
C     ---- AS IN ENGLOD, A FILE HOLDING EXACTLY MAXALR RECORDS IS NOT
C     ---- AN OVERFLOW.
  750 CONTINUE
      READ (LUALR,9010,END=700,ERR=800) CBUF
      GO TO 800
C
  700 CONTINUE
      CLOSE (LUALR)
      RETURN
  800 CONTINUE
      CALL ERRMSG (1040, 'ALERT TABLE OVERFLOW - TRUNCATED')
      CLOSE (LUALR)
      RETURN
  810 CONTINUE
      CALL ERRMSG (1041, 'ALERT RECORD REJECTED - BAD DATA')
      GO TO 100
  820 CONTINUE
      CALL ERRMSG (1042, 'ALERT RECORD FOR UNKNOWN SERIAL')
      GO TO 100
 9000 FORMAT (A8, 1X, I5, 1X, I1, 1X, I1, 1X, A4, 1X, F9.3, 1X,
     +        F9.3, 1X, A44)
 9010 FORMAT (A)
      END
C
      SUBROUTINE RULWR (LU, I, IMOD, NCYC, RISK)
C     ----------------------------------------------------------------
C     WRITE ONE LIFE PROJECTION RECORD.  IMOD IS THE LIMITING ENGINE
C     MODULE, NCYC THE CYCLES REMAINING ON IT AND RISK THE PROBABILITY
C     OF A REMOVAL WITHIN THE NEXT THREE HUNDRED CYCLES.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER LU, I, IMOD, NCYC
      REAL RISK
C
      WRITE (LU,9000) CESN(I), IRUL(I), IMOD, NCYC, HSCOR(I),
     +                ISTAT(I), RISK
      RETURN
 9000 FORMAT (A8, 1X, I6, 1X, I2, 1X, I6, 1X, F5.1, 1X, I1, 1X,
     +        F6.4)
      END
C
      SUBROUTINE RULLOD (JMOD, JCYC, RISK)
C     ----------------------------------------------------------------
C     LOAD THE LIFE PROJECTION FILE.  THE LIMITING MODULE, THE
C     CYCLES REMAINING ON IT AND THE REMOVAL RISK ARE RETURNED IN THE
C     CALLER SUPPLIED VECTORS; THE REMAINING FIELDS ARE PLACED BACK
C     INTO THE ENGINE MASTER TABLE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER JMOD(MAXENG), JCYC(MAXENG)
      REAL RISK(MAXENG)
      INTEGER I, J, N1, N2, N3, N4, ENGFND
      REAL SC, RK
      CHARACTER*8 CE
      EXTERNAL ENGFND
C
      DO 100 I = 1, NENG
         JMOD(I) = 0
         JCYC(I) = 0
         RISK(I) = 0.0
  100 CONTINUE
C
      CALL OPNSEQ (LURUL, 'RULMAS.DAT', 'OLD')
  200 CONTINUE
      READ (LURUL,9000,END=700,ERR=800) CE, N1, N2, N3, SC, N4, RK
      J = ENGFND(CE)
      IF (J .EQ. 0) GO TO 810
      IRUL(J) = N1
      JMOD(J) = N2
      JCYC(J) = N3
      HSCOR(J) = SC
      ISTAT(J) = N4
      RISK(J) = RK
      GO TO 200
C
  700 CONTINUE
      CLOSE (LURUL)
      RETURN
  800 CONTINUE
      CALL ERRMSG (1050, 'LIFE RECORD REJECTED - BAD DATA')
      GO TO 200
  810 CONTINUE
      CALL ERRMSG (1051, 'LIFE RECORD FOR UNKNOWN SERIAL')
      GO TO 200
 9000 FORMAT (A8, 1X, I6, 1X, I2, 1X, I6, 1X, F5.1, 1X, I1, 1X,
     +        F6.4)
      END
C
      SUBROUTINE SLTLOD (NSLT, CSLOT, CSBAS, ISWK, ISCAP, CSCAP)
C     ----------------------------------------------------------------
C     LOAD THE SHOP SLOT CAPACITY FILE.  ONE CARD PER SLOT, PUNCHED
C
C        COL 01-04  SLOT IDENTIFIER
C        COL 06-09  SHOP LOCATION, ICAO
C        COL 11-14  INDUCTION WEEK, FORM YYWW
C        COL 16-17  BAY CAPACITY IN ENGINES
C        COL 19-22  WORKSCOPE CAPABILITY, SEE THE SHOP MANUAL
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER NSLT, ISWK(MAXSLT), ISCAP(MAXSLT)
      CHARACTER*4 CSLOT(MAXSLT), CSBAS(MAXSLT), CSCAP(MAXSLT)
      CHARACTER*80 CCARD
      INTEGER I
C
      NSLT = 0
      CALL OPNSEQ (LUSLT, 'SHPSLT.DAT', 'OLD')
  100 CONTINUE
      READ (LUSLT,9000,END=700) CCARD
      IF (CCARD(1:1) .EQ. '*') GO TO 100
      IF (CCARD(1:4) .EQ. '    ') GO TO 100
      I = NSLT + 1
      IF (I .GT. MAXSLT) GO TO 800
      READ (CCARD,9010,ERR=810) CSLOT(I), CSBAS(I), ISWK(I),
     +                          ISCAP(I), CSCAP(I)
      NSLT = I
      GO TO 100
C
  700 CONTINUE
      CLOSE (LUSLT)
      IF (NSLT .EQ. 0) CALL ERRMSG (1060, 'SHOP SLOT FILE IS EMPTY')
      RETURN
  800 CONTINUE
      CALL ERRMSG (1061, 'SLOT TABLE OVERFLOW - CARDS IGNORED')
      CLOSE (LUSLT)
      RETURN
  810 CONTINUE
      CALL ERRMSG (1062, 'SLOT CARD REJECTED - BAD DATA')
      GO TO 100
 9000 FORMAT (A80)
 9010 FORMAT (A4, 1X, A4, 1X, I4, 1X, I2, 1X, A4)
      END
C
      SUBROUTINE PLNWR (LU, CE, CSLOT, CBASE, IWK, CWS, IPRI, NRUL)
C     ----------------------------------------------------------------
C     WRITE ONE SHOP VISIT PLAN RECORD.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER LU, IWK, IPRI, NRUL
      CHARACTER*8 CE
      CHARACTER*4 CSLOT, CBASE, CWS
C
      WRITE (LU,9000) CE, CSLOT, CBASE, IWK, CWS, IPRI, NRUL
      RETURN
 9000 FORMAT (A8, 1X, A4, 1X, A4, 1X, I4, 1X, A4, 1X, I2, 1X, I6)
      END
C
      SUBROUTINE PLNRD (LU, CE, CSLOT, CBASE, IWK, CWS, IPRI, NRUL,
     +                  IRC)
C     ----------------------------------------------------------------
C     READ ONE SHOP VISIT PLAN RECORD.  IRC IS KOK, KEOF OR KBAD.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER LU, IWK, IPRI, NRUL, IRC
      CHARACTER*8 CE
      CHARACTER*4 CSLOT, CBASE, CWS
C
      IRC = KOK
      READ (LU,9000,END=800,ERR=810) CE, CSLOT, CBASE, IWK, CWS,
     +                               IPRI, NRUL
      RETURN
  800 CONTINUE
      IRC = KEOF
      RETURN
  810 CONTINUE
      IRC = KBAD
      RETURN
 9000 FORMAT (A8, 1X, A4, 1X, A4, 1X, I4, 1X, A4, 1X, I2, 1X, I6)
      END
