      subroutine KEEL
     $(TE,N,NLIM,COOL,HEAT,TOTAL,YSML,YBIG,YL,YM,YU)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Gets plot limits, for HALYS.
C     !DASH
      save
C     !DASH
      real*8 COOL, HEAT, ONE, SIX, TE, TOTAL, YBGL, YBIG, YL, YM, YSML,
     $       YU, ZL, ZM, ZU
      integer I, N, NLIM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 7),SIX   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
C     !EJECT
      external  FEEL, KEEN, ABOVED, HI, BYE
      intrinsic max
C
C               COOL(N), HEAT(N), TOTAL(N), TE(N)
      dimension COOL(*), HEAT(*), TOTAL(*), TE(*)
C
      data ZL,ZM,ZU /1.D0, 2.8D1, 5.5D1/
C
      call HI ('KEEL')
C     !BEG
      call FEEL   (N, TE, NLIM)
      YSML = +ZZLARGE
      YBIG = -ZZLARGE
      do 100 I = 1,NLIM
        call KEEN (COOL(I),  YSML, YBIG)
        call KEEN (HEAT(I),  YSML, YBIG)
        call KEEN (TOTAL(I), YSML, YBIG)
  100 continue
C
      YBIG = max(YBIG,-YSML)
      YBGL = log10(YBIG)
      call ABOVED (YBGL, ONE, YBIG)
      YSML = YBIG-SIX
C
      YL = ZL
      YM = ZM
      YU = ZU
C     !END
      call BYE ('KEEL')
C
      return
      end
