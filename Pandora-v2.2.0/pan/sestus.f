      subroutine SESTUS
     $(SINT,WTAB,NLAM,NP,TOP,BOT,XLL,XRL,KODE)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Computes graph limits, for GELIMER.
C     Returns with KODE=1 if all seems OK, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 BOT, SINT, TOP, WTAB, XLL, XRL, YLL, YUL, ZERO
      integer KODE, NLAM, NP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
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
      external CHROME, TITAN, HI, BYE
C
C               WTAB(NLAM), SINT(NP,NLAM)
      dimension WTAB(*),    SINT(*)
C     !EJECT
C
      call HI ('SESTUS')
C     !BEG
      YUL = -ZZLARGE
      YLL = +ZZLARGE
      call CHROME  (NLAM,NP,SINT,1,ZERO,YUL,YLL)
C
      if((YUL.gt.YLL).and.(YUL.gt.ZERO).and.(YLL.gt.ZERO)) then
        KODE = 1
        call TITAN (YLL,YUL,BOT,TOP)
        if(WAVENO) then
          XLL = WTAB(NLAM)
          XRL = WTAB(1)
        else
          XLL = WTAB(1)
          XRL = WTAB(NLAM)
        end if
      else
        KODE = 0
      end if
C     !END
      call BYE ('SESTUS')
C
      return
      end
