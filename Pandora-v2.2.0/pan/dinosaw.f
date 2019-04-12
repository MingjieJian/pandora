      subroutine DINOSAW
     $(NO,LEGEND)
C
C     Rudolf Loeser, 1974 Mar 28
C---- Prints a heading, for BENJAMN.
C     !DASH
      save
C     !DASH
      integer LEGEND, NO
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
      external PRIAM, LINER, FORAGER, DOBRAWA, HI, BYE
C
      call HI ('DINOSAW')
C     !BEG
      if((NO.gt.0).and.(LEGEND.le.0)) then
        call PRIAM   (NO, 'CONTRIBUTORS', 12)
C
        call LINER   (2, NO)
        write (NO,100)
  100   format(' ','For each wavelength, the contributions to X are ',
     $             'shown in percent (truncated),'/
     $         ' ','at that depth which provides the largest ',
     $             'contribution to the central intensity integral.'//
     $         ' ','X can be: Total Opacity ("ABSORBERS"), and/or ',
     $             'Numerator of Absorption Source Function ',
     $             '("EMITTERS"),'/
     $         ' ','and/or Optical Depths ("TAUS"), - depending upon ',
     $             'options chosen.')
        call DOBRAWA (NO, WAVENO)
        call LINER   (3, NO)
C
        write (NO,101)
  101   format(' ','The individual contributors are numbered as ',
     $             'follows:')
        call LINER   (2, NO)
        call FORAGER (NO)
      end if
C     !END
      call BYE ('DINOSAW')
C
      return
      end
