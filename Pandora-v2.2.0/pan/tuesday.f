      subroutine TUESDAY
     $(X,W,XPBL,LU,BDI,XNE,HND,ZHEL,HNI,NLH)
C
C     Rudolf Loeser, 1975 Jul 29
C     RL/SGK revised Apr  9 2014 
C---- Calculates Electron Density using the "quadratic expression."
C     (This is version 2 of TUESDAY.)
C     !DASH
      save
C     !DASH
      real*8 BDI, EIDIF, HND, HNI, W, X, XNE, XPBL, ZHEL
      integer IETA, IHNP, IN, INEPR, IS, ISAB, ISUM, IU, JJAEL, JJRAB,
     $        JJRZM, JJSA, JJXNC, JJZME, JJZRN, LU, MOX, N, NLH, IZTRM,
     $        IZMER
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(236),JJZRN)
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
C     !DASH
C     !EJECT
      external JEZEBEL, SHERBET, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl), XNE(N), ZHEL(N), HNI(N,NLH), BDI(N,NL),
      dimension XPBL(*),      XNE(*), ZHEL(*), HNI(*),     BDI(*),
C
C               HND(N)
     $          HND(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IETA  ),(IN( 2),INEPR ),(IN( 3),ISUM  ),(IN( 4),IU    ),
     $(IN( 5),IHNP  ),(IN( 6),ISAB  ),(IN( 7),IZTRM ),(IN( 8),IZMER )
C
      call HI ('TUESDAY')
C     !BEG
C     (Get, and allocate, W allotment)
      call JEZEBEL (IN, IS, MOX, 'TUESDAY')
C
      call SHERBET (X, W, XPBL, N, NLH, XNE, HND, X(JJXNC), EIDIF,
     $              X(JJSA), X(JJRAB), HNI, X(JJRZM), X(JJAEL), BDI,
     $              X(JJZME), X(JJZRN), ZHEL, W(INEPR), W(IU), W(IHNP),
     $              W(ISAB), W(IETA), W(ISUM), LU, W(IZTRM), W(IZMER))
C
C     (Give back W allotment)
      call WGIVE   (W, 'TUESDAY')
C     !END
      call BYE ('TUESDAY')
C
      return
      end
