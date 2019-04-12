      subroutine CREASE
     $(R)
C
C     Rudolf Loeser, 2000 May 15
C---- Sets up an advisory, for CAESAR.
C     !DASH
      save
C     !DASH
      character BLANK*1, R*(*)
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
      call HI ('CREASE')
C     !BEG
      R = BLANK
      if(WHOLE) then
        R = '(Red and Blue halves should be averaged if they differ).'
      end if
C     !END
      call BYE ('CREASE')
C
      return
      end
