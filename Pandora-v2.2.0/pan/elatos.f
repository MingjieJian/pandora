      subroutine ELATOS
     $(NO,EMUI)
C
C     Rudolf Loeser, 1991 Jul 12
C---- Prints a heading for BRAID.
C     !DASH
      save
C     !DASH
      real*8 EMUI
      integer NO
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
      call HI ('ELATOS')
C     !BEG
      call LINER (3,NO)
      write (NO,100) EMUI,WLAB1(2:),WLAB4
  100 format(' ',8('-------'),'  ','Mu =',F7.4,'  ',8('-------')//
     $       ' ',23X,'Brightness'/
     $       ' ',4X,'W',A9,8X,'Temperature'/
     $       ' ',4X,A10,12X,'(K)')
      call LINER (1,NO)
C     !END
      call BYE ('ELATOS')
C
      return
      end
