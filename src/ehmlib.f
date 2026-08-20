C=====================================================================
C     EHMLIB   COMMON SUBROUTINE LIBRARY FOR THE EHMS SUITE
C              ROLLS-ROYCE PLC  -  CIVIL AERO SUPPORT  -  DERBY
C
C     CONTENTS
C        RANF     PSEUDO RANDOM GENERATOR, LEHMER, SCHRAGE METHOD
C        JULADD   ADD DAYS TO A JULIAN DATE OF FORM YYDDD
C        LINREG   LEAST SQUARES STRAIGHT LINE FIT
C        ISTATC   CLASSIFY A VALUE AGAINST AMBER AND RED LIMITS
C        SRTIDX   INDEX SORT, DESCENDING ON A REAL KEY
C        PAGHDR   PRINT A STANDARD PAGE HEADING
C        ERRMSG   WRITE A MESSAGE TO THE ERROR FILE
C        ENGFND   LOCATE AN ENGINE SERIAL IN THE MASTER TABLE
C        CSTAT    STATUS CODE TO PRINTABLE MNEMONIC
C        CSEV     SEVERITY CODE TO PRINTABLE MNEMONIC
C        CSRC     ALERT SOURCE CODE TO PRINTABLE MNEMONIC
C        CMODUL   ENGINE MODULE CODE TO PRINTABLE MNEMONIC
C=====================================================================
C
      REAL FUNCTION RANF (ISEED)
C     ----------------------------------------------------------------
C     UNIFORM PSEUDO RANDOM DEVIATE IN THE RANGE 0.0 TO 1.0.
C     MULTIPLICATIVE CONGRUENTIAL, MODULUS 2147483647, MULTIPLIER
C     16807, EVALUATED BY SCHRAGE FACTORISATION SO THAT NO PRODUCT
C     EVER EXCEEDS THE CAPACITY OF A 32 BIT WORD.  THE SEED IS
C     UPDATED IN PLACE, SO THE WHOLE SUITE REPRODUCES EXACTLY FROM
C     ONE RUN TO THE NEXT.
C     ----------------------------------------------------------------
      INTEGER ISEED
      INTEGER IA, IM, IQ, IR, IK
      PARAMETER (IA = 16807, IM = 2147483647)
      PARAMETER (IQ = 127773, IR = 2836)
C
      IF (ISEED .LE. 0) ISEED = 137
      IK = ISEED / IQ
      ISEED = IA * (ISEED - IK * IQ) - IR * IK
      IF (ISEED .LT. 0) ISEED = ISEED + IM
      RANF = FLOAT(ISEED) / FLOAT(IM)
      RETURN
      END
C
      INTEGER FUNCTION JULADD (JDATE, NDAYS)
C     ----------------------------------------------------------------
C     ADD NDAYS TO A DATE HELD IN THE FORM YYDDD.  LEAP YEARS ARE
C     RECOGNISED FOR THE YEARS 00 THROUGH 99 OF THIS CENTURY BY THE
C     USUAL DIVISIBLE BY FOUR RULE, WHICH IS SUFFICIENT FOR THE
C     REPORTING WINDOW OF THIS SYSTEM.
C     ----------------------------------------------------------------
      INTEGER JDATE, NDAYS
      INTEGER IYR, IDAY, NDYR
C
      IYR = JDATE / 1000
      IDAY = JDATE - IYR * 1000 + NDAYS
   10 CONTINUE
      NDYR = 365
      IF (MOD(IYR,4) .EQ. 0) NDYR = 366
      IF (IDAY .LE. NDYR) GO TO 20
      IDAY = IDAY - NDYR
      IYR = IYR + 1
      IF (IYR .GT. 99) IYR = 0
      GO TO 10
   20 CONTINUE
   30 IF (IDAY .GE. 1) GO TO 40
      IYR = IYR - 1
      IF (IYR .LT. 0) IYR = 99
      NDYR = 365
      IF (MOD(IYR,4) .EQ. 0) NDYR = 366
      IDAY = IDAY + NDYR
      GO TO 30
   40 CONTINUE
      JULADD = IYR * 1000 + IDAY
      RETURN
      END
C
      SUBROUTINE LINREG (X, Y, N, SLOPE, YINT, RSQ)
C     ----------------------------------------------------------------
C     LEAST SQUARES FIT OF Y = SLOPE * X + YINT OVER N PAIRS.
C     RSQ IS RETURNED AS THE COEFFICIENT OF DETERMINATION.  WHEN
C     FEWER THAN THREE POINTS ARE SUPPLIED, OR THE ABSCISSA HAS NO
C     SPREAD, THE FIT IS DECLARED FLAT AND RSQ IS SET TO ZERO.
C     ----------------------------------------------------------------
      INTEGER N
      REAL X(N), Y(N), SLOPE, YINT, RSQ
      INTEGER I
      REAL SX, SY, SXX, SXY, SYY, RN, DENOM, SSRES, SSTOT, YHAT
C
      SLOPE = 0.0
      YINT = 0.0
      RSQ = 0.0
      IF (N .LT. 3) GO TO 900
C
      SX = 0.0
      SY = 0.0
      SXX = 0.0
      SXY = 0.0
      SYY = 0.0
      DO 100 I = 1, N
         SX = SX + X(I)
         SY = SY + Y(I)
         SXX = SXX + X(I) * X(I)
         SXY = SXY + X(I) * Y(I)
         SYY = SYY + Y(I) * Y(I)
  100 CONTINUE
      RN = FLOAT(N)
      DENOM = RN * SXX - SX * SX
      IF (ABS(DENOM) .LT. 1.0E-6) GO TO 900
      SLOPE = (RN * SXY - SX * SY) / DENOM
      YINT = (SY - SLOPE * SX) / RN
C
      SSRES = 0.0
      SSTOT = 0.0
      DO 200 I = 1, N
         YHAT = SLOPE * X(I) + YINT
         SSRES = SSRES + (Y(I) - YHAT) ** 2
         SSTOT = SSTOT + (Y(I) - SY / RN) ** 2
  200 CONTINUE
      IF (SSTOT .LT. 1.0E-9) GO TO 900
      RSQ = 1.0 - SSRES / SSTOT
      IF (RSQ .LT. 0.0) RSQ = 0.0
  900 CONTINUE
      RETURN
      END
C
      INTEGER FUNCTION ISTATC (VALUE, AMBLIM, REDLIM, ISENSE)
C     ----------------------------------------------------------------
C     CLASSIFY ONE OBSERVATION AGAINST THE OPERATOR AND OEM LIMITS.
C     ISENSE SELECTS WHETHER A RISING OR A FALLING VALUE IS THE
C     ADVERSE DIRECTION.  THE RETURNED CODE IS KGREEN, KAMBER OR
C     KRED AS DECLARED IN EHMPRM.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      REAL VALUE, AMBLIM, REDLIM
      INTEGER ISENSE
C
      ISTATC = KGREEN
      IF (ISENSE .EQ. KLOBAD) GO TO 100
C     ---- RISING VALUE IS ADVERSE
      IF (VALUE .GE. AMBLIM) ISTATC = KAMBER
      IF (VALUE .GE. REDLIM) ISTATC = KRED
      GO TO 900
C     ---- FALLING VALUE IS ADVERSE
  100 CONTINUE
      IF (VALUE .LE. AMBLIM) ISTATC = KAMBER
      IF (VALUE .LE. REDLIM) ISTATC = KRED
  900 CONTINUE
      RETURN
      END
C
      SUBROUTINE SRTIDX (RKEY, INDX, N)
C     ----------------------------------------------------------------
C     BUILD AN INDEX VECTOR WHICH ORDERS RKEY IN DESCENDING SEQUENCE.
C     STRAIGHT INSERTION IS USED.  THE TABLES HANDLED BY THIS SYSTEM
C     ARE SMALL AND THE METHOD IS STABLE, WHICH KEEPS THE PRINTED
C     REPORTS REPEATABLE FROM RUN TO RUN.
C     ----------------------------------------------------------------
      INTEGER N, INDX(N)
      REAL RKEY(N)
      INTEGER I, J, ISAVE
      REAL RSAVE
C
      DO 100 I = 1, N
         INDX(I) = I
  100 CONTINUE
      IF (N .LT. 2) GO TO 900
C
      DO 300 I = 2, N
         ISAVE = INDX(I)
         RSAVE = RKEY(ISAVE)
         J = I - 1
  200    CONTINUE
         IF (J .LT. 1) GO TO 250
         IF (RKEY(INDX(J)) .GE. RSAVE) GO TO 250
         INDX(J+1) = INDX(J)
         J = J - 1
         GO TO 200
  250    CONTINUE
         INDX(J+1) = ISAVE
  300 CONTINUE
  900 CONTINUE
      RETURN
      END
C
      SUBROUTINE PAGHDR (LU, TITLE, IPAGE, NLINE)
C     ----------------------------------------------------------------
C     THROW TO THE HEAD OF FORM AND PRINT THE STANDARD TWO LINE
C     HEADING.  COLUMN ONE OF EVERY PRINT LINE IN THIS SYSTEM IS AN
C     ANSI CARRIAGE CONTROL CHARACTER AND IS NOT PART OF THE REPORT.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      INTEGER LU, IPAGE, NLINE
      CHARACTER*(*) TITLE
C
      IPAGE = IPAGE + 1
      WRITE (LU,9000) CRUNID, TITLE, JRUNDT, IPAGE
      WRITE (LU,9010)
      NLINE = 4
      RETURN
C
 9000 FORMAT ('1', A8, 2X, 'ROLLS-ROYCE PLC   ENGINE HEALTH ',
     +        'MONITORING SYSTEM   ', A, 5X, 'JULIAN ', I5,
     +        5X, 'PAGE ', I4)
 9010 FORMAT (' ', 128('='))
      END
C
      SUBROUTINE ERRMSG (MSGNO, TEXT)
C     ----------------------------------------------------------------
C     RECORD A DIAGNOSTIC ON THE ERROR FILE.  MESSAGE NUMBERS ARE
C     ALLOCATED IN THE OPERATIONS MANUAL, SECTION 7, AND MUST NOT BE
C     RE-USED BY A NEW PROGRAM.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER MSGNO
      CHARACTER*(*) TEXT
C
      WRITE (LUERR,9000) MSGNO, TEXT
      RETURN
 9000 FORMAT (' EHM', I4.4, 'E ', A)
      END
C
      INTEGER FUNCTION ENGFND (CKEY)
C     ----------------------------------------------------------------
C     LOCATE AN ENGINE SERIAL NUMBER IN THE CORE RESIDENT MASTER
C     TABLE.  THE TABLE IS HELD IN ASCENDING SERIAL SEQUENCE BY THE
C     LOAD STEP, SO A BINARY CHOP IS USED.  ZERO IS RETURNED WHEN THE
C     SERIAL IS NOT ON FILE.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      CHARACTER*8 CKEY
      INTEGER ILO, IHI, IMID
C
      ENGFND = 0
      ILO = 1
      IHI = NENG
  100 CONTINUE
      IF (ILO .GT. IHI) GO TO 900
      IMID = (ILO + IHI) / 2
      IF (CESN(IMID) .EQ. CKEY) GO TO 200
      IF (CESN(IMID) .LT. CKEY) ILO = IMID + 1
      IF (CESN(IMID) .GT. CKEY) IHI = IMID - 1
      GO TO 100
  200 CONTINUE
      ENGFND = IMID
  900 CONTINUE
      RETURN
      END
C
      CHARACTER*5 FUNCTION CSTAT (ICODE)
C     ----------------------------------------------------------------
C     PRINTABLE FORM OF A STATUS CODE.  RED MEANS ACT NOW, AMBER
C     MEANS WATCHLIST, GREEN MEANS NOMINAL AND NDAT MEANS THE
C     DOWNLINK COVERAGE WAS TOO POOR TO FORM A JUDGEMENT.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INTEGER ICODE
C
      CSTAT = 'NDAT '
      IF (ICODE .EQ. KGREEN) CSTAT = 'GREEN'
      IF (ICODE .EQ. KAMBER) CSTAT = 'AMBER'
      IF (ICODE .EQ. KRED)   CSTAT = 'RED  '
      RETURN
      END
C
      CHARACTER*8 FUNCTION CSEV (ISEV)
C     ----------------------------------------------------------------
C     PRINTABLE FORM OF AN ALERT SEVERITY CODE.
C     ----------------------------------------------------------------
      INTEGER ISEV
      CHARACTER*8 CTAB(5)
      DATA CTAB /'CRITICAL', 'HIGH    ', 'MEDIUM  ', 'LOW     ',
     +           'INFO    '/
C
      CSEV = 'UNKNOWN '
      IF (ISEV .GE. 1 .AND. ISEV .LE. 5) CSEV = CTAB(ISEV)
      RETURN
      END
C
      CHARACTER*8 FUNCTION CSRC (ISRC)
C     ----------------------------------------------------------------
C     PRINTABLE FORM OF AN ALERT SOURCE CODE.
C     ----------------------------------------------------------------
      INTEGER ISRC
      CHARACTER*8 CTAB(5)
      DATA CTAB /'EHM     ', 'ACARS   ', 'VIBRATN ', 'OILDEBR ',
     +           'TRENDMDL'/
C
      CSRC = 'UNKNOWN '
      IF (ISRC .GE. 1 .AND. ISRC .LE. 5) CSRC = CTAB(ISRC)
      RETURN
      END
C
      CHARACTER*9 FUNCTION CMODUL (IMOD)
C     ----------------------------------------------------------------
C     PRINTABLE FORM OF AN ENGINE MODULE CODE.  THE SEQUENCE FOLLOWS
C     THE GAS PATH FROM INTAKE TO EXHAUST AND THEN THE EXTERNALS.
C     ----------------------------------------------------------------
      INTEGER IMOD
      CHARACTER*9 CTAB(11)
      DATA CTAB /'FAN      ', 'IPC      ', 'HPC      ',
     +           'COMBUSTOR', 'HPT      ', 'IPT      ',
     +           'LPT      ', 'GEARBOX  ', 'ACCESSORY',
     +           'NACELLE  ', 'EXTERNALS'/
C
      CMODUL = 'UNKNOWN  '
      IF (IMOD .GE. 1 .AND. IMOD .LE. 11) CMODUL = CTAB(IMOD)
      RETURN
      END
C
      INTEGER FUNCTION LIMFND (CKEY)
C     ----------------------------------------------------------------
C     LOCATE A PARAMETER MNEMONIC IN THE LIMIT TABLE.  THE TABLE IS
C     SHORT AND UNSORTED, SO A SERIAL SEARCH IS USED.  ZERO IS
C     RETURNED WHEN NO LIMIT CARD HAS BEEN PUNCHED FOR THE
C     PARAMETER, IN WHICH CASE NO ALERT MAY BE RAISED AGAINST IT.
C     ----------------------------------------------------------------
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
      CHARACTER*4 CKEY
      INTEGER I
C
      LIMFND = 0
      DO 100 I = 1, NPRM
         IF (CPRM(I) .EQ. CKEY) LIMFND = I
  100 CONTINUE
      RETURN
      END
