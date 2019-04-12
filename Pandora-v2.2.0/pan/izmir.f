      subroutine IZMIR
     $(X,IX,NO,CTE,CNC)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Compares Hydrogen CE values.
C     !DASH
      save
C     !DASH
      real*8 CE, CNC, CTE, X
      integer I, IL, IU, IX, LIN, NL, NO
      logical DUMP, HYDR, lummy
      character VAL*14
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
C     !EJECT
      external LINER, SHIM, CELERY, AMOS, BECALM, DONNER, SINOP, DONAR,
     $         DONWAI, AMORPHA, AMPHORA, HI, BYE
C
      dimension X(*), IX(*)
C
      dimension CE(8), VAL(8)
C
      data DUMP,HYDR /.false., .true./
C
      call HI ('IZMIR')
C     !BEG
      call LINER       (1, NO)
      write (NO,100)
  100 format(' ','transition',6X,'this run',9X,'VREGE',8X,'SEATON',
     $           12X,'VS',7X,'JOHNSON',8X,'AGGRWL',12X,'PB',
     $           8X,'SCHOLZ')
      call LINER       (1, NO)
C
      LIN = 0
      do 103 IU = 2,NL
        do 102 IL = 1,(IU-1)
C
          call CELERY  (X, IX, IU, IL, CTE, CNC, DUMP, CE(1))
          call AMPHORA (X, IX, IU, IL, CTE, CE(2))
          call BECALM  (X, IX, IU, IL, CTE, HYDR, CE(3), lummy)
          call DONNER  (IU, IL, CTE, CNC, DUMP, CE(4))
          call DONWAI  (IU, IL, CTE, CNC, DUMP, CE(5))
          call AMOS    (IU, IL, CTE, CE(6))
          call AMORPHA (X, IX, IU, IL, CTE, CE(7))
          call DONAR   (IU, IL, CTE, CE(8))
C
          call SINOP   (8, CE, VAL)
          write (NO,101) IU,IL,VAL
  101     format(' ',5X,I2,'/',I2,8A14)
          LIN = LIN+1
          call SHIM    (LIN, 5, NO)
C
  102   continue
  103 continue
C     !END
      call BYE ('IZMIR')
C
      return
      end
