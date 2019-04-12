      subroutine LETTUCE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Allocates scratch storage for JEWEL.
C     (This is version 3 of LETTUCE.)
C     !DASH
      save
C     !DASH
      integer IMAX, IMIN, IN, IS, ISST, MUX, N, NBKITR, NCKITR, NL,
     $        NNDITR, NNEITR, NNHITR, NNKITR, NNT, NQHITR, NRHITR,
     $        NRKITR, NRWITR, NSSITR, NT, NTAITR, NTDITR, NZZITR
      character CALLER*(*)
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
      external  MINMAXI, WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('LETTUCE')
C     !BEG
      call WGET    (IS , CALLER)
C
      call MINMAXI (NITS, 1, NTMX, IMIN, IMAX)
      ISST = N*NITS(IMAX)
      NNT  = N*NT
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NCKITR*N*(NL-2)
      IN( 3) = IN( 2)+NTAITR*NNT
      IN( 4) = IN( 3)+NSSITR*NNT
      IN( 5) = IN( 4)+NRHITR*NNT
      IN( 6) = IN( 5)+NRWITR*NNT
      IN( 7) = IN( 6)+NNDITR*N*NL
      IN( 8) = IN( 7)+NRKITR*N
      IN( 9) = IN( 8)+NBKITR*N
      IN(10) = IN( 9)+NNEITR*N
      IN(11) = IN(10)+NZZITR*N
C
      IN(12) = IN(11)+NNKITR*N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+ISST
      IN(15) = IN(14)+NNHITR*N
      IN(16) = IN(15)+NTDITR*N
      MUX    = IN(16)+NQHITR*N
C
      call WLCK    (MUX,CALLER)
C     !END
      call BYE ('LETTUCE')
C
      return
      end
