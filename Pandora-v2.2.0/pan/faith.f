      subroutine FAITH
     $(X,W,IW,LUAP,DMPA,KVLG,N,Z,TE,ZT,XNE,HND,H2N,HEND,RHEAB,VM,HK,H1,
     $ HEK,HE1,HE2K,HE21,BETA,ZI,ZION,XION,Z1,Z2,Z3,ZXG,VAMB,VBMB,VCMB,
     $ VDMB,VE,VP,VH,V1,V2,V3)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Computes the ambipolar velocities:
C     VAMB, VBMB, VCMB, VDMB, VE, VP, VH, V1, V2, V3; and
C     the ionization terms: Z1, Z2, Z3, ZXG.
C     (This is version 3 of FAITH.)
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, H2N, HE1, HE21, HE2K, HEK, HEND, HK, HND, RHEAB,
     $       TE, V1, V2, V3, VAMB, VBMB, VCMB, VDMB, VE, VH, VM, VP, W,
     $       X, XION, XNE, Z, Z1, Z2, Z3, ZI, ZION, ZT, ZXG
      integer IDEE, IHCHCK, IN, IS, IVEMB, IW, KVLG, LUAP, MOX, N
      logical DMPA
C     !DASH
      external    FURAN, ERATO, FALCO, AQUAVIT, DILMUN, WGIVE, LIMPET,
     $            HI, BYE
C
      dimension   X(*), W(*), IW(*)
C
C                 VH(N), TE(N), ZT(N), XNE(N), HND(N), H2N(N), ZION(N),
      dimension   VH(*), TE(*), ZT(*), XNE(*), HND(*), H2N(*), ZION(*),
C
C                 HK(N), H1(N), Z2(N), HE1(N), HE2K(N), HE21(N), Z3(N),
     $            HK(*), H1(*), Z2(*), HE1(*), HE2K(*), HE21(*), Z3(*),
C
C                 VM(N), VP(N), ZI(N), Z1(N), HEK(N), BETA(N), XION(N),
     $            VM(*), VP(*), ZI(*), Z1(*), HEK(*), BETA(*), XION(*),
C
C                 Z(N), VBMB(N), VCMB(N), VDMB(N), VE(N), V1(N), V2(N),
     $            Z(*), VBMB(*), VCMB(*), VDMB(*), VE(*), V1(*), V2(*),
C
C                 VAMB(N), V3(N), RHEAB(N), HEND(N), ZXG(N)
     $            VAMB(*), V3(*), RHEAB(*), HEND(*), ZXG(*)
C
      dimension   IN(3)
      equivalence
     $(IN( 1),IHCHCK),(IN( 2),IDEE  ),(IN( 3),IVEMB )
C     !EJECT
C
      call HI ('FAITH')
C     !BEG
C     (Get, and allocate, W allotment)
      call FURAN    (IN, IS, MOX, 'FAITH')
C
C---- Compute Hydrogen ionization terms
      call LIMPET   (N, Z, HK, H1, ZI, ZION, XION, W, IW)
C
C---- Compute velocities and other terms
      call ERATO    (X, W, IW, N, Z, HK, H1, HEK, HE1, HE2K, HE21,
     $               BETA, TE, ZT, XNE, HND, H2N, HEND, RHEAB, VM,
     $               VAMB, VBMB, VCMB, VDMB, W(IVEMB), XION, ZION,
     $               ZI, Z1, Z2, Z3, ZXG, W(IDEE), KVLG, DMPA)
C---- Compute velocities VE, VP, VH, V1, V2, V3
      call FALCO    (N, HND, HK, H1, HEND, HEK, HE1, HE2K, BETA, XNE,
     $               VAMB, VBMB, VCMB, VDMB, V1, V2, V3, VP, VH, VE)
      if(DMPA) then
C----   Compute and print HECHECK [ ? ]
        call DILMUN (N, VM, V1, V2, V3, HE1, BETA, HE2K, W(IHCHCK))
      end if
C---- Print
      call AQUAVIT  (LUAP, DMPA, N, Z, TE, ZT, XNE, HND, HK, H1, HEK,
     $               HE1, HE2K, HE21, ZI, Z1, Z2, Z3, ZXG, VAMB, VBMB,
     $               VCMB, VDMB, VE, VP, VH, V1, V2, V3, XION, ZION,
     $               W(IDEE))
C
C     (Give baxk W allotment)
      call WGIVE    (W, 'FAITH')
C     !END
      call BYE ('FAITH')
C
      return
      end
