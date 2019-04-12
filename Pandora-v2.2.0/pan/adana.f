      subroutine ADANA
     $(X,IX,NO,CTE,CNC)
C
C     Rudolf Loeser, 2006 Apr 17
C---- Compares non-Hydrogen CE values.
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
      external LINER, SHIM, CELERY, AMPHORA, BECALM, SINOP, HI, BYE
C
      dimension X(*), IX(*)
C
      dimension CE(3), VAL(3)
C
      data HYDR,DUMP /.false., .false./
C
      call HI ('ADANA')
C     !BEG
      call LINER       (1, NO)
      write (NO,100)
  100 format(' ','transition',6X,'this run',9X,'VREGE',8X,'SEATON')
      call LINER       (1, NO)
C
      LIN = 0
      do 103 IU = 2,NL
        do 102 IL = 1,(IU-1)
C
          call CELERY  (X, IX, IU, IL, CTE, CNC, DUMP, CE(1))
          call AMPHORA (X, IX, IU, IL, CTE, CE(2))
          call BECALM  (X, IX, IU, IL, CTE, HYDR, CE(3), lummy)
C
          call SINOP   (3, CE, VAL)
          write (NO,101) IU,IL,VAL
  101     format(' ',5X,I2,'/',I2,3A14)
          LIN = LIN+1
          call SHIM    (LIN, 5, NO)
C
  102   continue
  103 continue
C     !END
      call BYE ('ADANA')
C
      return
      end
