      subroutine BRAMBLE
     $(X,IX,W,IW,LU)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Supervises number density dealings.
C     (This is version 2 of BRAMBLE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IABDL, IBD0, IBD1, IBDA, IBDD, IBDE, IBDN, IBDPR, IBDR,
     $        IBDUW, IBDW, IFION, IFLVS, IFRN, IIBS, IIKS, IIMG, IINS,
     $        IN, INDE, INDPR, INDS, INDUW, INDW, INKPR, INKUW, INKW,
     $        IRK, IRN, IS, IW, IWEIT, IWS, IX, IXPBL, JJBDI, JJBIJ,
     $        JJBTL, JJFON, JJFVS, JJH2N, JJHND, JJNK, JJNKS, JJSET,
     $        JJXND, JJXNE, JN, LU, LUD, LUE, LUF, LUH, LUN, LUP, MOX,
     $        MUX, N, NL, NSL, jummy
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
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 23),JJBIJ)
      equivalence (IZOQ( 51),JJNKS)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(154),JJFON)
      equivalence (IZOQ(155),JJFVS)
      equivalence (IZOQ(137),JJBTL)
      equivalence (IZOQ(253),JJSET)
C     !DASH
C     !EJECT
      external ULTAMI, WERNER, COTINGA, GORSE, POPIO, WGIVE, IGIVE,
     $         ITSY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(26)
      equivalence
     $(IN( 1),IBDPR ),(IN( 2),INDPR ),(IN( 3),INKPR ),(IN( 4),IBDUW ),
     $(IN( 5),IABDL ),(IN( 6),INKUW ),(IN( 7),INDUW ),(IN( 8),IXPBL ),
     $(IN( 9),INDS  ),(IN(10),IRK   ),(IN(11),IRN   ),(IN(12),IFRN  ),
     $(IN(13),IFION ),(IN(14),IFLVS ),(IN(15),IWEIT ),(IN(16),INDW  ),
     $(IN(17),IBDW  ),(IN(18),INDE  ),(IN(19),IBD0  ),(IN(20),IBD1  ),
     $(IN(21),IBDN  ),(IN(22),IBDD  ),(IN(23),IBDR  ),(IN(24),IBDE  ),
     $(IN(25),IBDA  ),(IN(26),INKW  )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IIKS  ),(JN( 2),IINS  ),(JN( 3),IIBS  ),(JN( 4),IIMG  )
C
      call HI ('BRAMBLE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ITSY    (IN, IS , MOX, 'BRAMBLE')
      call WERNER  (JN, IWS, MUX, 'BRAMBLE')
C
C     (Initialize populations buffer)
      call POPIO   ('INIT', jummy, W(IXPBL))
C---- Establish lfn's, and print header and explanations
      call COTINGA (LU, LUN, LUD, LUF, LUP, LUE, LUH)
C
C---- Compute
      call GORSE   (X, IX, W, IW, N, NL, NSL, LUE, LUN, LUD, LUF, LUP,
     $              X(JJBDI), X(JJXND), X(JJNK), X(JJXNE) ,X(JJBIJ),
     $              W(IBDPR), W(INDPR), W(INKPR), W(IBDUW), W(IBDW),
     $              W(INKUW), W(INDUW), W(INDW), W(INDE), X(JJNKS),
     $              W(INDS), W(IRK), W(IRN), X(JJHND), X(JJH2N),
     $              X(JJFON), X(JJFVS), X(JJBTL), W(IXPBL), IW(IIKS),
     $              IW(IINS), IW(IIBS), IW(IIMG), W(IABDL), W(IFRN),
     $              W(IFION), W(IFLVS), W(IWEIT), W(IBD0), W(IBD1),
     $              W(IBDN), W(IBDD), W(IBDR), W(IBDE), W(IBDA),
     $              W(INKW), X(JJSET))
C---- Print trailer
      call ULTAMI  (LUH, 'POPULATIONS')
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'BRAMBLE')
      call IGIVE   (IW, 'BRAMBLE')
C     !END
      call BYE ('BRAMBLE')
C
      return
      end
