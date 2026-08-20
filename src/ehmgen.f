C=====================================================================
C     EHMGEN   STEP 010 - FLEET AND DOWNLINK FILE GENERATOR
C
C     BUILDS THE ENGINE MASTER FILE AND A SYNTHETIC DOWNLINK LOG FOR
C     THE MANAGED FLEET.  THE GENERATOR IS DRIVEN ENTIRELY FROM THE
C     SEED ON THE CONTROL CARD, SO A GIVEN SEED ALWAYS REPRODUCES THE
C     SAME FLEET AND THE SAME PARAMETER HISTORY.  THIS IS RELIED UPON
C     BY THE ACCEPTANCE DECK IN DOCUMENT EHM/AT/011.
C
C     IN THE LIVE SERVICE THE ENGINE MASTER IS TAKEN FROM THE RECORDS
C     SYSTEM AND THE DOWNLINK LOG FROM THE ACARS GATEWAY TAPE.  THIS
C     STEP IS THEN REPLACED BY THE TAPE CONVERSION UTILITY.
C
C     FILES     ENGMAS  OUTPUT  ENGINE MASTER
C               FLTLOG  OUTPUT  FLIGHT DOWNLINK LOG
C               SYSERR  OUTPUT  DIAGNOSTICS
C=====================================================================
      PROGRAM EHMGEN
      INCLUDE 'EHMPRM.INC'
      INCLUDE 'EHMCOM.INC'
C
      INTEGER NFAM, NOPR, NSTN
      PARAMETER (NFAM = 6, NOPR = 8, NSTN = 14)
C
      CHARACTER*4 CFTAB(NFAM), CTTAB(NFAM)
      CHARACTER*2 COTAB(NOPR)
      CHARACTER*4 CBTAB(NOPR), CSTAB(NSTN)
      REAL EGTNEW(NFAM), FFBASE(NFAM)
      INTEGER NPFLT(MAXENG)
C
      INTEGER I, J, K, N, IENG, IFAM, IOPR, NFLT, NTOT, IDAY
      INTEGER IREJ, JD, IST, ISTN
      REAL V(NFVAL), RANF, R, DEGR, EM, HPC, DVIB, DOIL, DDEB
      EXTERNAL RANF
      CHARACTER*8 CE
      CHARACTER*6 CFLT
      CHARACTER*4 CORG, CDST
C
C     ---- ENGINE FAMILIES CARRIED BY THE MANAGED FLEET ---------------
      DATA CFTAB /'TXWA', 'TXWB', 'T10T', 'T700', 'T900', 'UFAN'/
      DATA CTTAB /'84KL', '97KL', '78KL', '68KL', '80KL', 'UF01'/
      DATA EGTNEW /78.0, 71.0, 66.0, 69.0, 54.0, 92.0/
      DATA FFBASE /2680.0, 2910.0, 2740.0, 2620.0, 3120.0, 2480.0/
C
C     ---- OPERATORS AND THEIR PRINCIPAL MAINTENANCE BASES -------------
      DATA COTAB /'BA', 'SQ', 'QR', 'EK', 'CX', 'LH', 'DL', 'QF'/
      DATA CBTAB /'EGLL', 'WSSS', 'OTHH', 'OMDB', 'VHHH', 'EDDF',
     +            'KATL', 'YSSY'/
C
C     ---- LINE STATIONS USED FOR SECTOR END POINTS -------------------
      DATA CSTAB /'EGLL', 'EGCC', 'EDDF', 'LFPG', 'EHAM', 'OMDB',
     +            'OTHH', 'WSSS', 'VHHH', 'RJTT', 'KJFK', 'KATL',
     +            'YSSY', 'FAOR'/
C
C     ================================================================
C     STEP 1.  INITIALISE AND READ THE CONTROL CARD
C     ================================================================
      CALL OPNSEQ (LUERR, 'SYSERR.DAT', 'NEW')
      CALL RDCTL
      WRITE (LUPRT,9000) CRUNID, JRUNDT, JSEED
C
      NENG = 168
      IF (NENG .GT. MAXENG) NENG = MAXENG
C
C     ================================================================
C     STEP 2.  BUILD THE ENGINE MASTER TABLE
C     ================================================================
      DO 200 I = 1, NENG
         IFAM = 1 + INT(RANF(JSEED) * FLOAT(NFAM))
         IF (IFAM .GT. NFAM) IFAM = NFAM
         IOPR = 1 + INT(RANF(JSEED) * FLOAT(NOPR))
         IF (IOPR .GT. NOPR) IOPR = NOPR
C
         WRITE (CESN(I),9100) 1000 + I
         CFAM(I) = CFTAB(IFAM)
         CTHR(I) = CTTAB(IFAM)
         COPR(I) = COTAB(IOPR)
         WRITE (CTAIL(I),9110) COTAB(IOPR), 1000 + I
         CLOCN(I) = CBTAB(IOPR)
C
C        ---- UTILISATION SINCE NEW -----------------------------------
         ITFC(I) = 180 + INT(RANF(JSEED) * 21000.0)
         HPC = 5.2 + RANF(JSEED) * 3.4
         TFH(I) = FLOAT(ITFC(I)) * HPC
C
C        ---- UTILISATION SINCE THE LAST SHOP VISIT -------------------
         ICSO(I) = 40 + INT(RANF(JSEED) * 5400.0)
         IF (ICSO(I) .GT. ITFC(I)) ICSO(I) = ITFC(I)
         HSO(I) = FLOAT(ICSO(I)) * HPC
C
C        ---- ROUTE ENVIRONMENT, ONE BENIGN TO FIVE HARSH -------------
         IENVS(I) = 1 + INT(RANF(JSEED) * 5.0)
         IF (IENVS(I) .GT. 5) IENVS(I) = 5
C
C        ---- LIFE STAGE AND INSTALLED POSITION -----------------------
         IST = 2
         IF (ITFC(I) .LT. 1500) IST = 1
         IF (ICSO(I) .GT. 4300) IST = 3
         IF (ICSO(I) .LT. 350 .AND. ITFC(I) .GT. 4000) IST = 5
         IF (RANF(JSEED) .LT. 0.04) IST = 4
         ISTG(I) = IST
         IPOSN(I) = 1 + INT(RANF(JSEED) * 2.0)
         IF (IPOSN(I) .GT. 2) IPOSN(I) = 2
         IF (IST .EQ. 4) IPOSN(I) = 0
         IF (IST .EQ. 4) CLOCN(I) = 'EGNX'
C
         WRITE (CBLD(I),9120) 1 + MOD(I,4), MOD(I,17)
C
C        ---- EXHAUST GAS TEMPERATURE MARGIN --------------------------
C        DETERIORATION IS DRIVEN BY CYCLES SINCE OVERHAUL, MODIFIED BY
C        THE SEVERITY OF THE ROUTE ENVIRONMENT, WITH A SMALL RESIDUAL
C        FOR TOTAL LIFE THAT NO OVERHAUL RECOVERS.
         DEGR = (0.0052 + 0.0016 * FLOAT(IENVS(I) - 1))
     +          * FLOAT(ICSO(I))
         EM = EGTNEW(IFAM) - DEGR - 0.00028 * FLOAT(ITFC(I))
         EM = EM + (RANF(JSEED) - 0.5) * 6.0
         IF (EM .LT. 1.5) EM = 1.5 + RANF(JSEED) * 2.0
         EGTM(I) = EM
C
C        ---- NUMBER OF SECTORS TO BE GENERATED FOR THIS ENGINE -------
         NPFLT(I) = 45 + INT(RANF(JSEED) * 45.0)
         IF (IST .EQ. 4) NPFLT(I) = 0
  200 CONTINUE
C
C     ---- WRITE THE ENGINE MASTER FILE -------------------------------
      CALL OPNSEQ (LUENG, 'ENGMAS.DAT', 'NEW')
      DO 300 I = 1, NENG
         CALL ENGWR (LUENG, I)
  300 CONTINUE
      CLOSE (LUENG)
C
C     ================================================================
C     STEP 3.  GENERATE THE DOWNLINK LOG, ENGINE MAJOR
C     ================================================================
      CALL OPNSEQ (LUFLT, 'FLTLOG.DAT', 'NEW')
      NTOT = 0
      IREJ = 0
C
      DO 500 I = 1, NENG
         NFLT = NPFLT(I)
         IF (NFLT .EQ. 0) GO TO 500
C
C        ---- PER ENGINE SIGNATURES -----------------------------------
C        A SMALL PROPORTION OF THE FLEET CARRIES A DEVELOPING FAULT.
C        THESE ARE THE ENGINES THE CONTROLLER MUST BE SHOWN FIRST.
         DVIB = 0.55 + RANF(JSEED) * 0.9
         IF (RANF(JSEED) .LT. 0.09) DVIB = 3.1 + RANF(JSEED) * 1.9
         DOIL = 0.12 + RANF(JSEED) * 0.28
         IF (RANF(JSEED) .LT. 0.11) DOIL = 0.62 + RANF(JSEED) * 0.5
         DDEB = 1.0 + RANF(JSEED) * 5.0
         IF (RANF(JSEED) .LT. 0.07) DDEB = 22.0 + RANF(JSEED) * 30.0
         DEGR = 0.0042 + 0.0013 * FLOAT(IENVS(I) - 1)
C
         DO 400 K = 1, NFLT
            N = NFLT - K
            IDAY = -2 * N
            JD = JULADD(JRUNDT, IDAY)
C
            IOPR = 1 + MOD(I, NOPR)
            WRITE (CFLT,9110) COPR(I), 100 + MOD(I * 7 + K, 8900)
            ISTN = 1 + MOD(I + K, NSTN)
            CORG = CSTAB(ISTN)
            ISTN = 1 + MOD(I + K + 5, NSTN)
            CDST = CSTAB(ISTN)
            IF (CDST .EQ. CORG) CDST = CSTAB(1 + MOD(ISTN, NSTN))
C
C           ---- SECTOR CONDITIONS ------------------------------------
            V(1) = 2.4 + RANF(JSEED) * 9.6
            V(2) = 4.0 + RANF(JSEED) * 21.0
            V(3) = -8.0 + RANF(JSEED) * 46.0
C
C           ---- EXHAUST GAS TEMPERATURE AND ITS MARGIN ---------------
            EM = EGTM(I) + DEGR * FLOAT(N) + (RANF(JSEED) - 0.5) * 2.4
            IF (EM .LT. 0.5) EM = 0.5
            V(5) = EM
            V(4) = 902.0 - EM + (RANF(JSEED) - 0.5) * 7.0
C
C           ---- SHAFT SPEEDS -----------------------------------------
            V(6) = 82.5 + RANF(JSEED) * 6.5
            V(7) = 90.0 + RANF(JSEED) * 6.0
            V(8) = 94.5 + RANF(JSEED) * 4.5
C
C           ---- VIBRATION, BROADBAND, PER SHAFT ----------------------
            V(9) = DVIB + (RANF(JSEED) - 0.4) * 0.35
     +             + 0.0016 * FLOAT(K)
            V(10) = 0.45 + RANF(JSEED) * 0.85
            V(11) = 0.35 + RANF(JSEED) * 0.75
            IF (V(9) .LT. 0.05) V(9) = 0.05
            IF (V(9) .GT. 9.99) V(9) = 9.99
C
C           ---- OIL SYSTEM -------------------------------------------
            V(12) = 44.0 + RANF(JSEED) * 14.0
            V(13) = 84.0 + RANF(JSEED) * 28.0
            V(14) = DOIL + (RANF(JSEED) - 0.5) * 0.12
            IF (V(14) .LT. 0.02) V(14) = 0.02
            V(16) = DDEB + (RANF(JSEED) - 0.5) * 4.0
     +              + 0.03 * FLOAT(K)
            IF (V(16) .LT. 0.0) V(16) = 0.0
            IF (V(16) .GT. 999.0) V(16) = 999.0
C
C           ---- FUEL BURN OVER THE SECTOR ----------------------------
            IFAM = 1
            DO 350 J = 1, NFAM
               IF (CFAM(I) .EQ. CFTAB(J)) IFAM = J
  350       CONTINUE
            R = 1.0 + 0.00035 * (EGTNEW(IFAM) - EM)
            V(15) = FFBASE(IFAM) * V(1) * R * (0.98 + RANF(JSEED)*0.04)
C
C           ---- HIGH PRESSURE TURBINE TIP CLEARANCE ------------------
            V(17) = 0.62 + 0.0055 * (EGTNEW(IFAM) - EM)
     +              + RANF(JSEED) * 0.09
            IF (V(17) .GT. 9.999) V(17) = 9.999
C
C           ---- DUST AND SAND EXPOSURE OVER THE SECTOR ---------------
            V(18) = 0.02 + 0.16 * FLOAT(IENVS(I) - 1)
     +              + RANF(JSEED) * 0.12
            IF (V(18) .GT. 0.99) V(18) = 0.99
C
            CE = CESN(I)
            CALL FLTWR (LUFLT, CE, JD, CFLT, CORG, CDST, V)
            NTOT = NTOT + 1
  400    CONTINUE
  500 CONTINUE
      CLOSE (LUFLT)
C
C     ================================================================
C     STEP 4.  RUN SUMMARY
C     ================================================================
      WRITE (LUPRT,9010) NENG, NTOT, IREJ
      IF (NTOT .GT. MAXFLT) CALL ERRMSG (2001,
     +   'DOWNLINK LOG EXCEEDS THE TREND WINDOW CAPACITY')
      CLOSE (LUERR)
      STOP
C
 9000 FORMAT ('1EHMGEN  RUN ', A8, '  JULIAN ', I5,
     +        '  SEED ', I10 /
     +        ' ', 60('-'))
 9010 FORMAT (' 0ENGINE MASTER RECORDS WRITTEN  ', I8 /
     +        ' DOWNLINK RECORDS WRITTEN       ', I8 /
     +        ' RECORDS REJECTED               ', I8 /
     +        ' 0EHM2000I STEP 010 COMPLETE')
 9100 FORMAT ('ESN-', I4.4)
 9110 FORMAT (A2, I4.4)
 9120 FORMAT ('B', I2.2, '.', I2.2)
      END
