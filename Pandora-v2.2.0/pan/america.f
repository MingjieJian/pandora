      subroutine AMERICA
     $(X,IX,W,IW,N,NL,RHOIJ,YBRIJ,RLI,RKI,CKI,GMI,CQSI,CIJ,GVL,KDGV,
     $ XM,D1L,DL1,XR,EP1,EP2,KOLEV)
C
C     Rudolf Loeser, 1968 Jun 12
C---- Computes the functions EP1 and EP2 for the Lyman calculations.
C     !DASH
      save
C     !DASH
      real*8 CIJ, CKI, CQSI, D1L, DL1, EP1, EP2, GMI, GVL, RHOIJ, RKI,
     $       RLI, W, X, XM, XR, YBRIJ
      integer I, IW, IX, KDGV, KOLEV, LDINT, M, N, NL
      logical DMPI, DUMP, KILROY
      character TITLE*40
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
      equivalence (KZQ( 48),LDINT)
C     !DASH
C     !EJECT
      external AFRICA, ASIA, EUROPE, BRAZIL, MINNA, MASHED, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               GVL(N,NL), EP2(N), RHOIJ(N,NT), RLI(N,NSL), RKI(N,NSL),
      dimension GVL(*),    EP2(*), RHOIJ(*),    RLI(*),     RKI(*),
C
C               CKI(N,NSL), GMI(N,NSL), CIJ(N,NL,NL), D1L(NL), DL1(NL),
     $          CKI(*),     GMI(*),     CIJ(*),       D1L(*),  DL1(*),
C
C               XR(NL-1,NL-1), YBRIJ(J(N,NT), CQSI(N,NL), XM(NL,NL),
     $          XR(*),         YBRIJ(*),      CQSI(*),    XM(*),
C
C               EP1(N)
     $          EP1(*)
C
      call HI ('AMERICA')
C     !BEG
      M = KOLEV
      call BRAZIL   (DUMP, KILROY, M, 'Nova', KDGV, 'AMERICA')
C
C---- Epsilons, one depth at a time
      do 100 I = 1,N
C----   Set up optional dump
        call MINNA  (DUMP, I, LDINT, DMPI)
C
C----   Compute the matrix M,
        call AFRICA (I, N, NL, RHOIJ, YBRIJ, CIJ, GMI, CQSI, RKI, CKI,
     $               KDGV, GVL, X, IX, XM)
C----   compute the determinant-vectors D1L and DL1,
        call ASIA   (XM, NL, XR, W, IW, D1L, DL1, TITLE, DMPI)
C----   and now compute EP1 and EP2.
        call EUROPE (I, N, NL, YBRIJ, CIJ, GMI, CQSI, RLI, CKI, X, IX,
     $               D1L, DL1, XM, EP1(I), EP2(I), GVL, KDGV, DMPI)
  100 continue
C
      if(.not.KILROY) then
        call MASHED ('AMERICA')
      end if
C     !END
      call BYE ('AMERICA')
C
      return
      end
