      subroutine FRUNZE
     $(NO,IMAGE)
C
C     Rudolf Loeser, 1991 Aug 09
C---- Prints the dI/dh plot, for OSTUNI.
C     !DASH
      save
C     !DASH
      integer NO
      character IMAGE*(*)
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
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('FRUNZE')
C     !BEG
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Plot of logs of runs of dI/dh vs. Z, for selected ',
     $           'wavelengths')
      call LINER  (1,NO)
C
      call KPRINT (IMAGE,NO)
C
      call LINER  (1,NO)
      write (NO,101) WLAB1
  101 format(' ','The ',A,' values corresponding to the different ',
     $           'plot symbols are identified in the preceding ',
     $           'printout.')
C     !END
      call BYE ('FRUNZE')
C
      return
      end
