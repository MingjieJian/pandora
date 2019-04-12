      subroutine DERMOT
     $(X,ABDEL,XNKW,XNDW,XNDE,BDIW,XNK,XND,BDI,FRN,FOLD,IMG)
C
C     Rudolf Loeser, 1998 Feb 06
C---- Controls renormalization of weighted number densities, and
C     associated editing of departure coefficients.
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BDI, BDIW, FOLD, FRN, X, XND, XNDE, XNDW, XNK, XNKW
      integer IMG, JJGM, JJHND, JJRAB, KOUNT, MBREC, N, NL, NNL
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
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 16),JJGM )
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
      equivalence (KZQ(195),MBREC)
C     !DASH
      external MOVE1, ELINOR, CLOD, HI, BYE
C
      dimension X(*)
C
C               XND(N,NL), XNDE(N,NL), XNDW(N,NL), BDI(N,NL), ABDEL(N),
      dimension XND(*),    XNDE(*),    XNDW(*),    BDI(*),    ABDEL(*),
C
C               BDIW(N,NL), XNK(N), FRN(N), XNKW(N), FOLD(N), IMG(N)
     $          BDIW(*),    XNK(*), FRN(*), XNKW(*), FOLD(*), IMG(*)
C     !EJECT
C
      call HI ('DERMOT')
C     !BEG
      NNL   = N*NL
      KOUNT = 0
C---- Renormalize NK and ND
      call MOVE1  (XNKW, N  , XNK)
      call MOVE1  (XNDE, NNL, XND)
      call ELINOR (N, NL, X(JJHND), X(JJRAB), ABDEL, FRN, XNK, XND,
     $             KOUNT, FOLD, IMG)
C
C---- Edit BDs accordingly
      call MOVE1  (BDIW, NNL, BDI)
      if(MBREC.eq.1) then
        call CLOD (N, NL, X(JJGM), XNDW, XND, BDIW, BDI, FOLD, IMG)
      end if
C     !END
      call BYE ('DERMOT')
C
      return
      end
