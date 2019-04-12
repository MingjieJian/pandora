      subroutine ARCTIC
     $(EP1,EP2,T,RHO,YBR,RL,RK,CK,CIJ,GM,CQS,GVL,KDGV,X,IX,W,IW)
C
C     Rudolf Loeser, 1981 Dec 14
C---- Computes Lyman Epsilons by a "NOVA"-like method.
C     (This is version 2 of ARCTIC.)
C     !DASH
      save
C     !DASH
      real*8 CIJ, CK, CQS, EP1, EP2, GM, GVL, RHO, RK, RL, T, TIN, TUT,
     $       W, X, YBR
      integer ID1L, IDL1, IN, IS, IW, IX, IXM, IXR, KDGV, KOLEV, MOX, N,
     $        NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C     !DASH
C     !EJECT
      external NOME, AMERICA, SECOND, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CK(N,NSL), RL(N,NSL), RK(N,NSL), GVL(N,NL), CQS(N,NSL),
      dimension CK(*),     RL(*),     RK(*),     GVL(*),    CQS(*),
C
C               GM(N,NSL), YBR(N,NT), RHO(N,NT), EP1(N), CIJ(N,NL,NL),
     $          GM(*),     YBR(*),    RHO(*),    EP1(*), CIJ(*),
C
C               EP2(N)
     $          EP2(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IXM   ),(IN( 2),ID1L  ),(IN( 3),IDL1  ),(IN( 4),IXR   )
C
      call HI ('ARCTIC')
C     !BEG
      call SECOND  (TIN)
C     (Get, and allocate, W allotment)
      call NOME    (IN, IS , MOX, 'ARCTIC')
C
      call AMERICA (X, IX, W, IW, N, NL, RHO, YBR, RL, RK, CK, GM, CQS,
     $              CIJ, GVL, KDGV, W(IXM), W(ID1L), W(IDL1), W(IXR),
     $              EP1, EP2, KOLEV)
C
C     (Give back W allotment)
      call WGIVE   (W, 'ARCTIC')
C
      call SECOND  (TUT)
      T = TUT-TIN
C     !END
      call BYE ('ARCTIC')
C
      return
      end
