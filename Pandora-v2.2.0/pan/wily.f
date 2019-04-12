      subroutine WILY
     $(X,IX,W,IW,BDIJ,METH,BDI,IMG,BD0,BD1,BDN,BDD,BDR,BDE)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Recovers individual BD sets from BD-ratios.
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDE, BDI, BDIJ, BDN, BDR, W, X, dummy
      integer IBDO, IBDX, IFO, IMG, IN, IS, IW, IX, IXVAL, JBFSW, JJCIJ,
     $        JJCK, JJGM, JJGVL, JJQS, JJQU, JJRHO, JJRK, JJRL, JJSQS,
     $        JJYBR, JJZIO, KDGV, METH, MOX, N, NL, NNL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ(223),JJSQS)
      equivalence (IZOQ( 19),JJQU )
      equivalence (IZOQ(175),JJGVL)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(203),JJZIO)
      equivalence (IZOQ( 20),JJQS )
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 46),JBFSW)
C
C---- ALTIMA      as of 2004 Mar 15
      real*8      ZZLALT,ZZSALT
      common      /ALTIMA/ ZZLALT,ZZSALT
C     Extreme values of NK, ND and BD.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(39),KDGV )
C     !DASH
      external LYNX, WOLGA, SPIKE, BADGER, JARAWA, ZERO1, MOVE1, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               BDIJ(N,NL), BD0(N,NL), BD1(N,NL), BDN(N,NL), BDD(N,NL),
      dimension BDIJ(*),    BD0(*),    BD1(*),    BDN(*),    BDD(*),
C
C               BDI(N,NL), BDR(N,NL), BDE(N,NL), IMG(N)
     $          BDI(*),    BDR(*),    BDE(*),    IMG(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IBDO  ),(IN( 2),IBDX  ),(IN( 3),IXVAL ),(IN( 4),IFO   )
C     !EJECT
C
      call HI ('WILY')
C     !BEG
C     (Get, and allocate, W allotment)
      call LYNX     (IN, IS, MOX, 'WILY')
C
      NNL = N*NL
      call ZERO1    (BD0, NNL)
      call ZERO1    (BD1, NNL)
      call ZERO1    (BDN, NNL)
      call ZERO1    (BDD, NNL)
      call ZERO1    (BDR, NNL)
      call ZERO1    (BDE, NNL)
C---- Compute raw new BD's
      if(NSL.le.NL) then
        call WOLGA  (X, IX, W, IW, N, NL, NSL, BDIJ, X(JJRHO),
     $               X(JJYBR), X(JJCIJ), X(JJGM), X(JJRK), X(JJRL),
     $               X(JJCK), dummy, X(JJQU), X(JJQS), X(JJSQS), KDGV,
     $               X(JJGVL), X(JJZIO), METH, BD0, BD1, BDN, BDD, BDR)
      else
C----   (As a temporary measure, force JBFSW = 1; 5/25/78)
        JBFSW = 1
        call SPIKE  (X, IX, W, IW, N, NL, NSL, BDIJ, X(JJRHO),
     $               X(JJYBR), X(JJCIJ), X(JJGM), X(JJRK), X(JJRL),
     $               X(JJCK), BDR, METH, BD0, BD1, BDN, BDD, W(IBDX),
     $               W(IBDO), JBFSW, X(JJGVL), KDGV, X(JJQU), X(JJQS),
     $               X(JJSQS), X(JJZIO))
      end if
C---- Final editing, to remove values .le. zero
      call MOVE1    (BDR, NNL, BDE)
      call JARAWA   (N, NL, BDE, IMG, W(IFO))
C---- Final smoothing
      call MOVE1    (BDE, NNL, BDI)
      call BADGER   (N, NL, BDI, W(IXVAL), W, IW)
C
C     (Give back W allotment)
      call WGIVE    (W, 'WILY')
C     !END
      call BYE ('WILY')
C
      return
      end
