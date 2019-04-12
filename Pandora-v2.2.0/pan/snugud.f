      subroutine SNUGUD
     $(NW,WAVES,CORE,WVNUM,WTAB,LINE)
C
C     Rudolf Loeser, 2000 Sep 14
C---- Sets up WVNUM and WTAB.
C     !DASH
      save
C     !DASH
      real*8 CORE, WAVES, WTAB, WVNUM
      integer LINE, NW
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
      external DUNGUS, WUMBLE, HI, BYE
C
C               WAVES(NW+), WVNUM(NW+), WTAB(NW+)
      dimension WAVES(*),   WVNUM(*),   WTAB(*)
C
      call HI ('SNUGUD')
C     !BEG
C---- Set up table of wavenumbers corresponding to WAVES+(CORE)
      call DUNGUS (WAVES, CORE, NW, WVNUM)
C
C---- Set up WTAB, and parameters in common block MOSTAR
      call WUMBLE (NW, WAVES, WVNUM, WTAB, CORE, LINE)
C     !END
      call BYE ('SNUGUD')
C
      return
      end
