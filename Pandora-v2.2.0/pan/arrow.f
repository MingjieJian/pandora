      subroutine ARROW
     $(ITAU,JUP,KLO,KRJ,RHOIJ,YBRIJ,X,IX,TERM)
C
C     Rudolf Loeser, 1978 Nov 28
C---- Calculates the term:  A(JUP,KLO)*RHO(ITAU,JUP,KLO).
C
C     KRJ = 1 means: input consists of RHO with JBAR;
C         = 2      : input consists of JBAR only;
C         = 3      : input consists of RHO only.
C
C     (This is version 2 of ARROW.)
C     !DASH
      save
C     !DASH
      real*8 RHOIJ, TERM, X, YBRIJ, ZERO
      integer ITAU, IX, JJAIJ, JJALF, JJFIN, JJKIJ, JJLIJ, JJSET, JJXIN,
     $        JJXNU, JJYBC, JJZ, JUP, KLO, KRJ
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 81),JJYBC)
      equivalence (IZOQ(253),JJSET)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ(  8),JJLIJ)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MESA, HI, BYE
C
      dimension X(*), IX(*)
C
C               RHOIJ(N,NT), YBRIJ(N,NT)
      dimension RHOIJ(*),    YBRIJ(*)
C     !EJECT
C
      call HI ('ARROW')
C     !BEG
      if(JUP.le.KLO) then
        TERM = ZERO
      else
        call MESA (ITAU, JUP, KLO, KRJ, RHOIJ, YBRIJ, X(JJAIJ),
     $             IX(JJKIJ), IX(JJLIJ), X(JJALF), X(JJYBC), X(JJSET),
     $             X(JJXNU), X(JJZ), X(JJXIN), X(JJFIN), TERM)
      end if
C     !END
      call BYE ('ARROW')
C
      return
      end
