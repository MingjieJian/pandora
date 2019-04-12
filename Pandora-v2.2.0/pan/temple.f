      subroutine TEMPLE
     $(EP1,EP2,T,CK,CIJ,RS,BDI,XND,RHO,YBR,GM,GVL,KDGV,X,IX,W,IW)
C
C     Rudolf Loeser, 1981 Dec 14
C---- Computes Lyman Epsilons by a "CMPLEX-LOWER"-like method.
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, CK, EP1, EP2, GM, GVL, RHO, RS, T, TIN, TUT, W,
     $       X, XND, YBR
      integer IARHO, ICINC, IFLM, IN, IQL, IQLI, IS, ISUMS, ISUMU, IW,
     $        IX, KDGV, KOLEV, MOX, N, NL
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
      external COBENO, ALTAR, SECOND, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               GVL(N,NL), EP2(N), BDI(N,NL), EP1(N), YBR(N,NT), RS(N),
      dimension GVL(*),    EP2(*), BDI(*),    EP1(*), YBR(*),    RS(*),
C
C               XND(N,NL), RHO(N,NT), GM(N,NSL), CK(N,NL), CIJ(N,NL,NL)
     $          XND(*),    RHO(*),    GM(*),     CK(*),    CIJ(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),ISUMU ),(IN( 2),ISUMS ),(IN( 3),IQL   ),(IN( 4),IARHO ),
     $(IN( 5),ICINC ),(IN( 6),IFLM  ),(IN( 7),IQLI  )
C     !EJECT
C
      call HI ('TEMPLE')
C     !BEG
      call SECOND (TIN)
C     (Get, and allocate, W allotment)
      call COBENO (IN, IS, MOX, 'TEMPLE')
C
      call ALTAR  (N, NL, KOLEV, EP1, EP2, CK, RS, BDI, XND, RHO, YBR,
     $             GM, CIJ, W(ISUMS), W(ISUMU), W(IARHO), W(ICINC),
     $             W(IFLM), W(IQLI), W(IQL), X, IX, GVL, KDGV)
C
C     (Give back W allotment)
      call WGIVE  (W, 'TEMPLE')
C
      call SECOND (TUT)
      T = TUT-TIN
C     !END
      call BYE ('TEMPLE')
C
      return
      end
