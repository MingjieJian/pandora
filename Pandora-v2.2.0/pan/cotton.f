      subroutine COTTON
     $(IB,IE,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Writes a depths-group heading, for MODEL.
C     (This is version 2 of COTTON.)
C     !DASH
      save
C     !DASH
      integer I, IB, IE, NO
      character DASH*10
C     !DASH
      external LINER, HI, BYE
C
      data DASH /'----------'/
C
      call HI ('COTTON')
C     !BEG
      call LINER (2,NO)
      write (NO,100) (DASH,I=IB,IE)
  100 format(' ',39X,8A10)
      write (NO,101) (I   ,I=IB,IE)
  101 format(' ',39X,8I10)
C     !END
      call BYE ('COTTON')
C
      return
      end
