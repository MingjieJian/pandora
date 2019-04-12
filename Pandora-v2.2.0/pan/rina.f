      subroutine RINA
     $(X,IX,W,RKI,IQRK,RLI,IQRL,LUP,LUG,LUS)
C
C     Rudolf Loeser, 1969 Dec 22
C---- Supervises the computation of Rates from Jnu.
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X
      integer IFA, IFB, IN, IOMX, IOVER, IQCCR, IQRK, IQRL, IRCP, IRCPP,
     $        IRKM, IRKMC, IRLA, IRLAC, IRLB, IRLBC, IRLM, IRLMC, IRLS1,
     $        IRLSN, IRNU, IRNUP, IS, ITR, ITREF, ITRS, IUJ, IUPJ, IWT,
     $        IX, IXCBL, IYR, IYW, IYWS, JJCK, JJCP, JJMRJ, JJPKS,
     $        JJRKQ, JJRLQ, JJRRC, JJRRN, JJTE, JJTR, JJXCU, JJXNU,
     $        JJYRA, JJZ, JPOP, KKJNU, KSHEL, LIM, LUG, LUP, LUS, MOX,
     $        N, NLIM, NNSL, NSL
      logical KOOL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 69),JJRRN)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 77),JJYRA)
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ( 66),JJRKQ)
      equivalence (IZOQ( 88),JJPKS)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ( 67),JJRLQ)
      equivalence (IZOQ(260),JJXCU)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  1),JJMRJ)
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
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ(  6),IRLS1)
      equivalence (KZQ(  9),IRLSN)
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(22),JPOP )
      equivalence (LEST( 1),KSHEL)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
C     !EJECT
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
      equivalence (IQQ(133),IQCCR)
C     !DASH
      external  LEILA, ZERO1, MANU, RIVALLO, MOVE1, WGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), IX(*), W(*)
C
C               IQRL(NSL), RLI(N,NSL), IQRK(NSL), RKI(N,NSL)
      dimension IQRL(*),   RLI(*),     IQRK(*),   RKI(*)
C
      dimension IN(24)
      equivalence
     $(IN( 1),IRNU  ),(IN( 2),IRCP  ),(IN( 3),IYW   ),(IN( 4),IFA   ),
     $(IN( 5),IFB   ),(IN( 6),ITR   ),(IN( 7),IRKM  ),(IN( 8),IRLM  ),
     $(IN( 9),IYR   ),(IN(10),IRNUP ),(IN(11),IRCPP ),(IN(12),ITRS  ),
     $(IN(13),IYWS  ),(IN(14),IUJ   ),(IN(15),IUPJ  ),(IN(16),IRLA  ),
     $(IN(17),IRLB  ),(IN(18),IWT   ),(IN(19),IRKMC ),(IN(20),IRLMC ),
     $(IN(21),IRLAC ),(IN(22),IRLBC ),(IN(23),ITREF ),(IN(24),IXCBL )
C     !EJECT
C
      call HI ('RINA')
C     !BEG
C     (Get, and allocate, W allotment)
      call LEILA   (IN, IS, MOX, 'RINA')
C
      KOOL = ((IQCCR.gt.0).and.((IOVER.eq.IOMX).or.(JPOP.eq.0)))
      LIM  = NSL+min(KSHEL,1)
      NNSL = N*NSL
      NLIM = N*LIM
C---- Initialize buffers for data to be plotted
      call ZERO1   (W(ITRS),  NLIM)
      call ZERO1   (W(IYWS),  NLIM)
      call ZERO1   (W(ITREF), NLIM)
C
C---- Compute RKI and RLI by continuum integrations
      call MANU    (LUP, LUG, LUS, NSL, LIM, N, IX(JJMRJ), X(JJRRN),
     $              X(JJRRC), W(IRNU), W(IRCP), X(JJXNU), X(JJXCU),
     $              W(IYW), W(IFA), W(IFB), W(ITR), IQRK, IQRL, RKI,
     $              W(IRKM), RLI, W(IRLM), W(IRLA), W(IRLB), X(JJCP),
     $              X(JJTE), X(JJYRA), W(IYR), W(IRNUP), W(IRCPP),
     $              W(ITRS), W(IYWS), W(IUJ), W(IUPJ), W(IXCBL),
     $              IRLS1, IRLSN, X(JJCK), KOOL, X(JJRKQ), W(IRKMC),
     $              X(JJRLQ), W(IRLMC), W(IRLAC), W(IRLBC), W(ITREF),
     $              X(JJPKS), W(IWT), W)
C
C---- Save computed TR-effective
      call MOVE1   (W(ITREF), NNSL, X(JJTR))
C---- Make plots
      call RIVALLO (LUG, W(ITRS), W(IYWS), W(ITREF), LIM, X(JJZ), N,
     $              W(ITR), X(JJTE))
C
C     (Give back W allotment)
      call WGIVE   (W, 'RINA')
C     !END
      call BYE ('RINA')
C
      return
      end
