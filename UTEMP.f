!
      SUBROUTINE UEXTERNALDB(LOP,LRESTART,TIME,DTIME,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION TIME(2)
C
C****PROCEDURES FOR *TEMPERATURE,USER
C************************************
C***************Basic Variables******
C MTI - Array Size for time points
C MN -  Array Size for Node labels
C NTI - Number of time points
C TI(I) - Time points
C TE(I,N) - Temperature
C************************************
C
C*********************************************
C******** Reading of file ********************
C*********************************************
C
       PARAMETER (TEMFILE_UNIT=99, MTI=500, MN=514586, Temp0=15)
       CHARACTER STR*80,CHARV(2)*8
       DIMENSION INTV(2),REALV(2)
       POINTER::TI(:),TE(:,:)
       INTEGER(2) TimeI
       INTEGER(1) FLAG
       COMMON /SSS/ NTI,TimeI,FLAG,TI,TE
C
      IF((LOP.EQ.0).OR.(LOP.EQ.4)) THEN
         ALLOCATE (TI(MTI))
         ALLOCATE (TE(MTI,MN))
         FLAG = 1
C
         OPEN (TEMFILE_UNIT,FILE='/temp/Scripts/Temp.dat',STATUS='OLD', READONLY, IOSTAT=IOS)
         INTV(1)=IOS
         IF (IOS.NE.0) CALL STDB_ABQERR(-3,'UEXTERNALDB:Error %I opening .tem file',INTV,REALV,CHARV)
C *** Reading data file
         I = 0
 1000    READ(TEMFILE_UNIT,'(A)',END=2000) STR
         iPos=index(STR, 'Time=' )
         IF(iPos.NE.0) THEN
           I = I + 1
           IF(I.GT.MTI) CALL STDB_ABQERR(-3,'UEXTERNALDB:Number of time increments higher than maximum allowed',INTV,REALV,CHARV)
             READ(STR(iPos+5:),*) TI(I)
             WRITE(6,"('UTEMP: I,TI(I)=',I4,F14.1)") I,TI(I)
           ELSE
           IF(I.EQ.0) THEN
C *** Initialize time point
               I     = 1
               TI(1) = 1.
           ENDIF
           READ(STR,*) N, TE(I,N)
           IF(N.GT.MN) CALL STDB_ABQERR(-3,'UEXTERNALDB:Node label greater than maximum allowed',INTV,REALV,CHARV)
           ENDIF
           GOTO 1000
 2000      CLOSE(TEMFILE_UNIT)
C
           NTI = I
           TI(0) = 0.
C           TI(NTI+1) = 600000
           DO J = 1, MN
               TE(0,J) = Temp0
C               TE(NTI+1,J) = TE(NTI,J)
           ENDDO
           TimeI = 0
           INTV(1) = NTI
           CALL STDB_ABQERR(1,'UEXTERNALDB: %I time points have been loaded',INTV,REALV,CHARV)
      ELSEIF(LOP.EQ.2)THEN
         FLAG = 1
      ENDIF
C
      RETURN
C
      END
C
C*********************************************
C********Temperature in nodes*****************
C*********************************************
      SUBROUTINE UTEMP(TEMP,NSECPT,KSTEP,KINC,TIME,NODE,COORDS)
C
      INCLUDE 'ABA_PARAM.INC'
      POINTER::TI(:),TE(:,:)
      INTEGER(2) TimeI
      INTEGER(1) FLAG
      COMMON /SSS/ NTI,TimeI, FLAG, TI, TE 
C
      DIMENSION TEMP(NSECPT), TIME(2), COORDS(3)
C
      CHARACTER CHARV(2)*8
      DIMENSION INTV(2),REALV(2)
      DOUBLE PRECISION XI, CURRENTTIME
C
      SAVE J,XI
C
        CURRENTTIME = TIME(2)
        IF (FLAG.EQ.1) THEN
           IF (CURRENTTIME.LT.TI(NTI)) THEN
             DO WHILE (TI(TimeI+1).LT.CURRENTTIME)
               TimeI = TimeI+1
             ENDDO
             XI = (CURRENTTIME - TI(TimeI)) / (TI(TimeI+1) - TI(TimeI))
             J = TimeI + 1
           ELSE
               TimeI = NTI
               J = NTI
               XI = 0
           ENDIF
           FLAG = 0
        ENDIF
        TE1 = TE(TimeI,NODE) + XI * (TE(J,NODE) - TE(TimeI,NODE))
        TEMP(1) = TE1
C
      RETURN
      END
