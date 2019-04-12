      subroutine NANDI
     $(X,N,XL,XR,OK)
C
C     Rudolf Loeser, 1982 Apr 28
C---- Gets logarithmic axis limits for plots.
C     (This is version 2 of NANDI.)
C     !DASH
      save
C     !DASH
      real*8 X, XL, XMAX, XMIN, XR, ZERO
      integer I, N
      logical OK
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  TITAN, HI, BYE
      intrinsic min,max
C
C               X(N)
      dimension X(*)
C
      call HI ('NANDI')
C     !BEG
      XMIN = +ZZLARGE
      XMAX = -ZZLARGE
C
      do 100 I = 1,N
        if(X(I).gt.ZERO) then
          XMIN = min(XMIN,X(I))
          XMAX = max(XMAX,X(I))
        end if
  100 continue
C
      OK = ((XMIN.gt.ZERO).and.(XMAX.gt.ZERO))
C
      if(OK) then
        call TITAN (XMIN, XMAX, XL, XR)
      end if
C     !END
      call BYE ('NANDI')
C
      return
      end
