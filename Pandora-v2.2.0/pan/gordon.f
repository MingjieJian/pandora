      subroutine GORDON
     $(NW,NZE,WTAB,WAVLOG,XIS,NO)
C
C     Rudolf Loeser, 1993 Jun 16
C---- Plots, for GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 WAVLOG, WTAB, XIS, XL, XR, ZERO, dummy
      integer I, KODE, NO, NW, NZE, jummy
      character LABEL*17
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
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
C     !EJECT
      external LOGO, DANCE, RAMA, HI, BYE
C
C               WTAB(NW), WAVLOG(NW), XIS(NW,NZE)
      dimension WTAB(*),  WAVLOG(*),  XIS(*)
C
      call HI ('GORDON')
C     !BEG
C---- Make logarithmic abscissa table (set =0 values to be omitted)
      call LOGO   (WTAB, NW, 1, ZERO, WAVLOG)
C---- Determine abscissa limits
      call DANCE  (WAVLOG, NW, XL, XR, KODE)
      if(KODE.gt.0) then
C----   Plot
        LABEL = 'log10(W'//WLAB1(2:)//')'
        call RAMA (NO, NZE, NW, WAVLOG, XL, XR, LABEL, XIS, 0, dummy,
     $             jummy)
      end if
C     !END
      call BYE ('GORDON')
C
      return
      end
