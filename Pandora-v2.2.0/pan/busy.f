      subroutine BUSY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 May 21
C---- Controls the reading of the 2., 3. and 4. batches
C     of input statements: - general input;
C                            spectrum calculations input; and
C                            populations data.
C     (This is version 4 of BUSY.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IW, IWS, IX, IXIINP, IXIKSW, IXILZA, IXINLP,
     $        JJHND, JJRAB, JJRBL, JJTDN, JJTR, JJXNE, JJZ, JN, MOX,
     $        MUX, N, NDBL, NFR, NKPCR, NL, NLB1, NLB3, NPSHF, NVEC,
     $        NWAVC, NWGHT, NZAUX, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ(189),JJRBL)
C
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
      external ESMERAL, OCTAVIA, POPPOP, FLOSS, NADINE, TOPAZ, CHECKER,
     $         SAUCER, RUM, POMP, MONK, SABOT, JEER, JIB, ZEROI, ZERO1,
     $         QUARTZ, POPIO, IGIVE, WGIVE, MALLARD, CELLAR, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),NPSHF ),(IN( 2),NZAUX ),(IN( 3),NWAVC ),(IN( 4),NKPCR ),
     $(IN( 5),NLB3  ),(IN( 6),NDBL  ),(IN( 7),NVEC  ),(IN( 8),NLB1  ),
     $(IN( 9),NFR   ),(IN(10),NWGHT )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IXILZA),(JN( 2),IXINLP),(JN( 3),IXIKSW),(JN( 4),IXIINP)
C     !EJECT
C
      call HI ('BUSY')
C     !BEG
C---- (Get, and allocate, W & IW allotments; and initialize them)
      call MONK    (IN, IS , MOX, 'BUSY')
      call JEER    (JN, IWS, MUX, 'BUSY')
      call ZERO1   (W (IS) ,    (MOX-IS +1))
      call ZEROI   (IW(IWS), 1, (MUX-IWS+1))
C
C---- Move data from P into permanent slots, and make integer versions
C     of LZA and NLPAIR
      call JIB     (X, IX, W(NPSHF), IW(IXILZA), IW(IXINLP))
C---- Defaults for Batch 2
      call FLOSS   (X, IX)
C
C
C---- Read Batch 2 of input - general data,  A N D
C     do post-read defaults for Batch 2,  A N D
C---- initialize Line Intensity Data Blocks
C
      call NADINE  (X, IX, W, IW, W(NLB1), W(NDBL), W(NLB3), IW(IXILZA),
     $              W(NZAUX), W(NKPCR), IW(IXIINP), IW(IXINLP),
     $              W(NWAVC), IW(IXIKSW))
C
C
C---- Allocate data blocks
      call CELLAR
C
C---- Read Batch 3 of input - spectrum calculations data
      call OCTAVIA (X, IX)
C---- Post-read defaults for Batch 3
      call RUM     (X, IX, W(NFR), W(NVEC), W(NWGHT))
C
C---- Read Batch 4 of input - populations data
      call POPIO   ('INIT', jummy, W(NDBL))
      call POMP    (X, W, W(NDBL), IW(IXILZA), W(NZAUX))
C---- Post-read defaults for Batch 4
      call SAUCER  (X, W)
C     !EJECT
C---- Save Z, NE, NH and TDST for iterative summary
      call QUARTZ  (X(JJZ))
      call TOPAZ   (X(JJXNE), 1)
      call SABOT   (X(JJHND))
      call ESMERAL (X(JJTDN))
C---- Debug checksums
      call CHECKER (X(JJZ), 1, N, 'Input Z')
      call CHECKER (X(JJTR), 1, (N*NL), 'Input TR')
      call CHECKER (X(JJXNE), 1, N, 'Input NE')
      call CHECKER (X(JJHND), 1, N, 'Input NH')
C---- Print random access file indices (if needed)
      call MALLARD ('BUSY')
      call POPPOP
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'BUSY')
      call IGIVE   (IW, 'BUSY')
C     !END
      call BYE ('BUSY')
C
      return
      end
