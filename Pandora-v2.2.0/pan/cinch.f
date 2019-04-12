      subroutine CINCH
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 Apr 02
C---- Updates CIJ when FCE has changed, when a full-blown
C     Rates Calculation is not required.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JJCIA, JJCIJ, JJFCE, JJFCJ, JJHIJ, JJKIJ, JJSIJ,
     $        N, NL, NO
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
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(252),JJSIJ)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(251),JJFCJ)
      equivalence (IZOQ(207),JJCIA)
      equivalence (IZOQ(250),JJHIJ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !DASH
      external GUEST, NEST, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NO /0/
C
      call HI ('CINCH')
C     !BEG
C---- Get CIJ
      call GUEST (N, NL, X(JJFCE), IX(JJKIJ), X(JJSIJ), X(JJCIJ))
C
C---- Provide final adjustments
      call NEST  (NO, N, NL, X(JJFCJ), X(JJCIA), X(JJHIJ), X(JJCIJ))
C     !END
      call BYE ('CINCH')
C
      return
      end
