      subroutine ORKAN
     $(NO,IS,IE)
C
C     Rudolf Loeser, 1985 Feb 07
C---- Prints wavelength indices, for TYPHOON.
C     !DASH
      save
C     !DASH
      integer I, IE, IS, NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ORKAN')
C     !BEG
      call LINER (1,NO)
      if(IE.le.99) then
        write (NO,100) (I,I=IS,IE,1)
  100   format(' ',7X,40I3)
      else if(IE.le.99999) then
        write (NO,101) (I,I=IS,IE,2)
  101   format(' ',7X,20I6)
      else
        write (NO,102) (I,I=IS,IE,4)
  102   format(' ',7X,10I12)
      end if
      call LINER (1,NO)
C     !END
      call BYE ('ORKAN')
C
      return
      end
