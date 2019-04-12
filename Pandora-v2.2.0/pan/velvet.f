      subroutine VELVET
     $(WVL,DDL,FDDL,CDL,LDL,DL,K,DP,DW,V,XNE,N,LU,W,IW)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Drives the printing of details from the calculation of a value of
C     a Hydrogen line absoprtion profile with Stark broadening.
C     (This is version 2 of VELVET.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DL, DP, DW, FDDL, V, W, WVL, XNE
      integer IAA, IDP, IDV, IHSDD, IHSDP, IN, IPHC, IPHI, IS, IUU, IW,
     $        K, LDL, LU, MOX, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(124),IHSDP)
      equivalence (KZQ(125),IHSDD)
C     !DASH
      external IDAS, MOVED, VICUNA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DDL(LDL), CDL(LDL), XNE(N), DP(N,LDL), DW(N), FDDL(N),
      dimension DDL(*),   CDL(*),   XNE(*), DP(N,*),   DW(*), FDDL(*),
C
C               V(N), DL(K)
     $          V(*), DL(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),IDV   ),(IN( 3),IAA   ),(IN( 4),IUU   ),
     $(IN( 5),IPHC  ),(IN( 6),IDP   )
C
      call HI ('VELVET')
C     !BEG
C     (Get, and allocate, W allotment)
      call IDAS   (IN, IS, MOX, 'VELVET')
C
      call MOVED  (DP(IHSDP,1), N, LDL, W(IDP), 1, LDL)
      call VICUNA (WVL, DDL, FDDL, CDL, LDL, DL(IHSDD), 1, W(IDP),
     $             DW(IHSDP), V(IHSDP), XNE(IHSDP), 1, W(IPHI),
     $             W(IDV), W(IAA), W(IUU), 1, W(IPHC), LU, W, IW)
C
C     (Give back W allotment)
      call WGIVE  (W, 'VELVET')
C     !END
      call BYE ('VELVET')
C
      return
      end
