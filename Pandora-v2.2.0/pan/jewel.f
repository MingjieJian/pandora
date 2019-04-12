      subroutine JEWEL
     $(X,W)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Supervises iteration summaries.
C     (This is version 2 of JEWEL.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IAAA, IBDK, ICHK, IDST, IHND, IN, IPRT, IQHI, IQICK,
     $        IQIDP, IQSMT, IQSUM, IRHO, IRHW, IRKK, IS, ISSS, ITAU,
     $        IXND, IXNE, IXNK, IZZZ, MOX, N, NCK, NL, NO, NT
      logical DMP, DOSUM
      character LINES*88
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ( 18),IQSUM)
      equivalence (IQQ(284),IQSMT)
      equivalence (IQQ(302),IQICK)
      equivalence (IQQ(142),IQIDP)
C     !EJECT
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
C     .
      equivalence
     $(NITS( 1),NTAITR),(NITS( 2),NCKITR),(NITS( 3),NRHITR),
     $(NITS( 4),NRKITR),(NITS( 5),NNDITR),(NITS( 6),NRWITR),
     $(NITS( 7),NBKITR),(NITS( 8),NNEITR),(NITS( 9),NQHITR),
     $(NITS(10),NZZITR),(NITS(11),NSSITR),(NITS(12),NNHITR),
     $(NITS(13),NTDITR),(NITS(14),NNKITR)
C     .
C     !DASH
C     !EJECT
      external LETTUCE, RIBEIRO, RAVEN, FRAME, RAVE, WGIVE, NICE, GLUM,
     $         RAMP, VANE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension LINES(MXIBIS)
C
      dimension IN(16)
      equivalence
     $(IN( 1),ICHK  ),(IN( 2),ITAU  ),(IN( 3),ISSS  ),(IN( 4),IRHO  ),
     $(IN( 5),IRHW  ),(IN( 6),IXND  ),(IN( 7),IRKK  ),(IN( 8),IBDK  ),
     $(IN( 9),IXNE  ),(IN(10),IZZZ  ),(IN(11),IXNK  ),(IN(12),IPRT  ),
     $(IN(13),IAAA  ),(IN(14),IHND  ),(IN(15),IDST  ),(IN(16),IQHI  )
C
      call HI ('JEWEL')
C     !BEG
C     (Get, and allocate, W allotment)
      call LETTUCE  (IN, IS, MOX, 'JEWEL')
C
C---- Fudger summary
      call GLUM     (NO)
C
C---- A-troubles summary
      call RAMP     (X, NO)
C
      call RIBEIRO  (NO, DOSUM)
      if(DOSUM) then
        DMP = IQIDP.gt.0
        NCK = NL-2
C----   Read data
        call RAVEN  (N, NL, NCK, NT, W(ICHK), W(ITAU), W(ISSS),
     $               W(IRHO), W(IRHW), W(IXND), W(IRKK), W(IBDK),
     $               W(IXNE), W(IZZZ), W(IHND), W(IDST), W(IXNK),
     $               W(IQHI), DMP, LINES)
C
C----   Make Checks Graph
        call FRAME  (W(ICHK), N, NL, NCK, NO)
C
        if((IQSUM.gt.0).or.(IQSMT.gt.0)) then
C----     Make general iterative summaries (and dump ?)
          call RAVE (NO)
          call VANE (DMP, NO, NIBIS, LINES, N, NL, NCK, NT)
          call NICE (N, NL, NCK, NT, W(ICHK), W(ITAU), W(ISSS),
     $                W(IRHO), W(IRHW), W(IXND), W(IRKK), W(IBDK),
     $                W(IXNE), W(IZZZ), W(IHND), W(IDST), W(IXNK),
     $                W(IAAA), W(IPRT), W(IQHI), NO)
        end if
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'JEWEL')
C     !END
      call BYE ('JEWEL')
C
      return
      end
