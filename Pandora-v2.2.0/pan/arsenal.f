      subroutine ARSENAL
     $(I,IU,IL,N,NL,LAST,X,IX,W,Z)
C
C     Rudolf Loeser, 2003 Nov 24
C---- Computes VAMOS-type Z, for debug printout.
C     (This is version 2 of ARSENAL.)
C     !DASH
      save
C     !DASH
      real*8 W, X, Z
      integer I, ICIJ, IGMI, IL, IN, IPIJ, IS, ISAIJ, IU, IX, IYBRIJ,
     $        IYIJ, JJCIJ, JJGM, JJQHI, JJQSA, JJQST, JJYBR, KKPIJ, MOX,
     $        N, NL
      logical LAST
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(246),JJQSA)
      equivalence (IZOQ(247),JJQST)
      equivalence (IZOQ( 13),JJYBR)
C     !DASH
      external AUSTRAL, SALAD, PACUTE, ZIA, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               Z(NL,NL)
      dimension Z(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IPIJ  ),(IN( 2),ICIJ  ),(IN( 3),ISAIJ ),(IN( 4),IYIJ  ),
     $(IN( 5),IYBRIJ),(IN( 6),IGMI  )
C
      call HI ('ARSENAL')
C     !BEG
C     (Get, and allocate, W allotment)
      call AUSTRAL (IN, IS, MOX, 'ARSENAL')
C
      call SALAD   (LAST, KKPIJ)
      call PACUTE  (I, N, NL, X(KKPIJ), X(JJCIJ), X(JJGM), X(JJQHI),
     $              X(JJQSA), X(JJQST), X(JJYBR), W(IPIJ), W(ICIJ),
     $              W(IGMI), W(ISAIJ), W(IYIJ), W(IYBRIJ))
      call ZIA     (IU, IL, NL, W(ICIJ), W(IPIJ), W(IYIJ), W(IYBRIJ),
     $              X, IX, Z)
C
C     (Give back W allotment)
      call WGIVE   (W, 'ARSENAL')
C     !END
      call BYE ('ARSENAL')
C
      return
      end
