      subroutine TOTO
     $(NO,LAB,IS,IE,INC)
C
C     Rudolf Loeser, 2001 Oct 16
C---- Prints a heading for eclipse profile printouts.
C     !DASH
      save
C     !DASH
      integer I, IE, INC, IS, NO
      character LAB*3
C     !COM
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
      external LINER, HI, BYE
C
      call HI ('TOTO')
C     !BEG
      if(NO.gt.0) then
        call LINER (2,NO)
        write (NO,100) WLAB5,(LAB,I,I=IS,IE,INC)
  100   format(' ',8X,A2,7X,10(2X,A3,'(',I3,') ',:))
        call LINER (1,NO)
      end if
C     !END
      call BYE ('TOTO')
C
      return
      end
