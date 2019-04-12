      subroutine ASTRID
     $(X,W,HND,RZM,AEL,XNE,XNC,ZME,ZRN,ETA,HNK,HNKR,ZHEL,ITER,LU,DUMP)
C
C     Rudolf Loeser, 1980 Jul 29
C     RL/SGK revised Apr  9 2014
C---- Sets up a new Electron Density and associated quantities.
C     (This is version 3 of ASTRID.)
C     !DASH
      save
C     !DASH
      real*8 AEL, EIDIF, ETA, HND, HNK, HNKR, RZM, W, X, XNC, XNE, ZHEL,
     $       ZME, ZRN
      integer IN, INEPR, IQNES, IS, ITER, IXPBL, LU, MOX, N, IZTRM, 
     $        IZMER
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ( 57),EIDIF)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 56),IQNES)
C     !EJECT
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !DASH
      external MALLET, HELGA, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XNE(N), HNK(N), ZME(N), HND(N), RZM(N), XNC(N), AEL(N),
      dimension XNE(*), HNK(*), ZME(*), HND(*), RZM(*), XNC(*), AEL(*),
C
C               ETA(N,NMT), ZHEL(N), HNKR(N), ZRN(N)
     $          ETA(*),     ZHEL(*), HNKR(*), ZRN(*)
C
      dimension IN(4)
      equivalence
     $(IN (1),INEPR ),(IN( 2), IXPBL ),(IN( 3), IZTRM ),(IN( 4), IZMER )
C     !EJECT
C
      call HI ('ASTRID')
C     !BEG
C     (Get, and allocate, W allotment)
      call MALLET (IN, IS, MOX, 'ASTRID')
C
      call HELGA  (X, W, W(IXPBL), EIDIF, N, NMT, LU, XNC, XNE, HNK,
     $             ZME, HND, RZM, AEL, W(INEPR), HNKR, ETA, ZRN, ITER,
     $             IQNES, ZHEL, DUMP, W(IZTRM), W(IZMER))
C
C     (Give back W allotment)
      call WGIVE  (W, 'ASTRID')
C     !END
      call BYE ('ASTRID')
C
      return
      end
