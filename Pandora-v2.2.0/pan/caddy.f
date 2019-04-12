      subroutine CADDY
     $(ITAU, JLO,KUP, KRJ,IU,IL, YBRIJ,X,IX,TERM)
C
C     Rudolf Loeser, 1978 Dec 01
C---- Calculates the "Term added to the upward C",
C     i.e. the term accompanying C(ITAU,JLO,KUP), KUP .gt. JLO.
C                                > Not for VAMOS!
C
C---- KRJ=1 means - input consists of "RHO and JBAR" (see ARROW);
C     KRJ=2 means - input consists of "JBAR only";
C     KRJ=3 means - input consists of "RHO only".
C
C---- (IU,IL) are the transition indices when PE(u,l) and FE(u,l)
C     are being computed; they should be (0,0) otherwise.
C
C     See also TEACUP.
C     !DASH
      save
C     !DASH
      real*8 TERM, X, YBRIJ, ZERO
      integer IL, ITAU, IU, IX, JJAIJ, JJALF, JJFIN, JJKIJ, JJLIJ, JJP,
     $        JJXIN, JJXNU, JJYBC, JJZ, JLO, KRJ, KUP
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 81),JJYBC)
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
C     !EJECT
      external MUSA, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBRIJ(N,NT)
      dimension YBRIJ(*)
C
      call HI ('CADDY')
C     !BEG
      if(KUP.le.JLO) then
        TERM = ZERO
      else
        call MUSA (ITAU, KUP, JLO, KRJ, IU, IL, YBRIJ, X(JJAIJ),
     $             IX(JJKIJ), IX(JJLIJ), X(JJP), X(JJALF), X(JJYBC),
     $             X(JJXNU), X(JJZ), X(JJXIN), X(JJFIN), TERM)
      end if
C     !END
      call BYE ('CADDY')
C
      return
      end
