      subroutine HEP
     $(I,IU,IL,N,NL,KRJ,LAST,X,IX,Z)
C
C     Rudolf Loeser, 1982 Jul 08
C---- Get Z at depth I, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 X, Z
      integer I, IL, IU, IX, JJBDI, JJCIJ, JJGM, JJKIJ, JJRHO, JJWEI,
     $        JJYBR, KKPIJ, KRJ, N, NL
      logical LAST
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ( 16),JJGM )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !DASH
      external SALAD, ZEA, HI, BYE
C
      dimension X(*), IX(*)
C
C               Z(NL,NL)
      dimension Z(*)
C
      call HI ('HEP')
C     !BEG
      call SALAD (LAST, KKPIJ)
      call ZEA   (I, IU, IL, N, NL, KRJ, X(JJCIJ), X(KKPIJ), X(JJGM),
     $            X(JJBDI), X(JJRHO), IX(JJKIJ), X(JJWEI), X(JJYBR),
     $            X, IX, Z)
C     !END
      call BYE ('HEP')
C
      return
      end
