      subroutine CHOLULA
     $(JU,JL,MTR,NO)
C
C     Rudolf Loeser, 1982 May 11
C---- Prints a heading, for PICTURE.
C     !DASH
      save
C     !DASH
      integer I, JL, JU, MTR, NO
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               JU(MTR), JL(MTR)
      dimension JU(*),   JL(*)
C
      call HI ('CHOLULA')
C     !BEG
      call ABJECT (NO)
      write (NO,100) (JU(I),JL(I),I=1,MTR)
  100 format(' ','The TAU scales.',98X,'(Option SCALE)'///
     $       ' ',2X,'TS',6X,11(:1X,'TAU',1X,I2,',',I2))
      call LINER  (1,NO)
C     !END
      call BYE ('CHOLULA')
C
      return
      end
