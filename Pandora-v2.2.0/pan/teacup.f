      subroutine TEACUP
     $(X,IX,W,IW,N,NL,K,L,PE,FE,GMI,CIJ,PIJ,CHI,SA,ASTAR,YBR,Z,XM,XR,
     $ SM,DI,XS,DUMP)
 
C     Rudolf Loeser, 2003 Nov 19
C---- Computes Statistical Equilibrium terms PE and FE,
C     for the VAMOS method.
C
C     See also TRIP.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, CHI, CIJ, D, DI, FE, GMI, ONE, OOG, PE, PIJ, SA, SM,
     $       W, X, XM, XR, XS, XX, YBR, YY, Z, ZM11
      integer ICIJ, IGMI, IN, IPIJ, IS, ISAIJ, ITAU, IW, IX, IYBRIJ,
     $        IYIJ, K, L, MOX, N, NL, NLM
      logical DMPI, DUMP, KILROY
      character TITLE*50
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, TORT, GAG, TRUE, SKULL, MASHED, PACUTE, WGIVE,
     $         LUSA, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               PE(N), FE(N), GMI(N,NSL), XR(NL-1), Z(NL,NL), DI(NL-1),
      dimension PE(*), FE(*), GMI(N,*),   XR(*),    Z(*),     DI(*),
C
C               CIJ(N,NL**2), PIJ(N,NL**2), YBR(N,NT), XM(NL-1,NL-1),
     $          CIJ(*),       PIJ(*),       YBR(*),    XM(*),
C
C               XS(NL-1,NL-1), CHI(N,NT), SA(N,NT), ASTAR(N,NT),
     $          XS(*),         CHI(*),    SA(*),    ASTAR(*),
C
C               SM(NL-1)
     $          SM(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IPIJ  ),(IN( 2),ICIJ  ),(IN( 3),IGMI  ),(IN( 4),ISAIJ ),
     $(IN( 5),IYIJ  ),(IN( 6),IYBRIJ)
C
      call HI ('TEACUP')
C     !BEG
C     (Get, and allocate, W allotment)
      call LUSA     (IN, IS, MOX, 'TEACUP')
C
      NLM    = NL-1
      KILROY = .true.
C     !EJECT
C---- Loop over all depths
      do 101 ITAU = 1,N
C       ( ? initialize dump)
        call SKULL  (DUMP, KILROY, 'TEACUP', 'VAMOS', K, L, ITAU, DMPI)
C
C----   Extract data at this depth
        call PACUTE (ITAU, N, NL, PIJ, CIJ, GMI, CHI, SA, ASTAR, YBR,
     $               W(IPIJ), W(ICIJ), W(IGMI), W(ISAIJ), W(IYIJ),
     $               W(IYBRIJ))
C----   Get modified matrices and little-M11
        call TORT   (XM, XR, SM, K, L, N, NL, NLM, W(IGMI), W(ICIJ),
     $               W(IPIJ), W(IYBRIJ), W(ISAIJ), W(IYIJ), Z, ZM11,
     $               X, IX, DMPI)
C----   Get determinants
        call GAG    (NL, XM, D, DI, XS, TITLE, W, IW)
C----   Get XX and YY
        call TRUE   (DI, XX, YY, XM, XR, NLM, D, ZM11, W, IW, ITAU,
     $               DMPI)
C
C----   And now compute PE and FE
        call DIVIDE (ONE, GMI(ITAU,K), OOG)
        PE(ITAU) = XX*OOG
        FE(ITAU) = YY*OOG
  101 continue
      if(.not.KILROY) then
C       ( ? close dump)
        call MASHED ('TEACUP')
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'TEACUP')
C     !END
      call BYE ('TEACUP')
C
      return
      end
