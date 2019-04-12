      subroutine WALBIRI
     $(X,W,XNE,ZME,HND,ZHEL,HNKR,ETA,NEITER,MEITER,DUMP)
C
C     Rudolf Loeser, 1980 Jul 22
C---- Computes new electron density, for the H.S.E. calculation.
C     !DASH
      save
C     !DASH
      real*8 ETA, HND, HNKR, W, X, XNE, ZHEL, ZME, dummy
      integer JJAEL, JJRZM, JJXNC, JJZRN, LU, MEITER, N, NEITER
      logical DUMP
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
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ(236),JJZRN)
C     !DASH
      external ASTRID, WENDY, HI, BYE
C
      dimension X(*), W(*)
C
C               ETA(N,NMT), XNE(N), ZME(N), HND(N), ZHEL(N), HNKR(N)
      dimension ETA(*),     XNE(*), ZME(*), HND(*), ZHEL(*), HNKR(*)
C
      data LU /0/
C
      call HI ('WALBIRI')
C     !BEG
C---- Compute
      call ASTRID (X, W, HND, X(JJRZM), X(JJAEL), XNE, X(JJXNC), ZME,
     $             X(JJZRN), ETA, dummy, HNKR, ZHEL, NEITER, LU, DUMP)
      MEITER = MEITER+NEITER
C
C---- Continuum Recalculation control
      call WENDY  (XNE, 1, N, 3, 'WALBIRI')
C     !END
      call BYE ('WALBIRI')
C
      return
      end
