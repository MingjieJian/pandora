      subroutine FLIVVER
     $(N,HNDO,HNDN,G,H,Z,W,F,V,C)
C
C     Rudolf Loeser, 1980 Nov 03
C---- Computes F, V and C, control parameters for the H.S.E. HND
C     calculation, for PLEASE.
C     !DASH
      save
C     !DASH
      real*8 ARG, ARGL, C, CLNH, F, G, H, HII, HNDN, HNDO, OLNH, ONE,
     $       RGI, RHNI, RHOI, V, W, Z
      integer KZERO, N
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
      equivalence (RZQ( 70),CLNH )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(10),KZERO)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external GIGUE, DIVIDE, LOTUS, HI, BYE
C
C               HNDO(N), HNDN(N), G(N), H(N), Z(N), W(N)
      dimension HNDO(*), HNDN(*), G(*), H(*), Z(*), W(*)
C
      call HI ('FLIVVER')
C     !BEG
C---- Form F
      call GIGUE    (HNDO, W, N, Z, KZERO, RHOI)
      call GIGUE    (HNDN, W, N, Z, KZERO, RHNI)
      call DIVIDE   (RHNI, RHOI, F)
C
C---- Compute V
      OLNH = ONE/CLNH
      if(F.gt.CLNH) then
        V = CLNH
      else if(F.lt.OLNH) then
        V = OLNH
      else
        V = ONE
      end if
C
C---- Compute C
      if(V.eq.ONE) then
        C = ONE
      else
        call GIGUE  (G, W, N, Z, KZERO, RGI)
        call LOTUS  (N, Z, H, KZERO, HII)
        call DIVIDE ((V*RHOI), RGI, ARG)
        ARGL = log(ARG)
        call DIVIDE (ARGL, HII, C)
      end if
C     !END
      call BYE ('FLIVVER')
C
      return
      end
