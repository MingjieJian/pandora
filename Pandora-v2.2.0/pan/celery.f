      subroutine CELERY
     $(X,IX,IU,IL,TE,DENS,DUMP,CE)
C
C     Rudolf Loeser, 2006 Apr 12
C---- Drives CEREAL to compute CE(IU,IL,TE,DENS).
C     !DASH
      save
C     !DASH
      real*8 CE, DENS, TE, X
      integer IL, IU, IX, JJAAT, JJACE, JJAIJ, JJCEI, JJLRQ, JJMCE,
     $        JJNPQ, JJP, JJTER, JJXCU, JJXNU
      logical DUMP
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ(262),JJAAT)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(107),JJACE)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 32),JJAIJ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
C     !DASH
      external CEREAL, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('CELERY')
C     !BEG
      call CEREAL (IU, IL, X(JJTER), X(JJCEI), TE, DENS, X(JJAIJ),
     $             X(JJAAT), X(JJXNU), X(JJXCU), X(JJP), IX(JJNPQ),
     $             IX(JJLRQ), X(JJMCE), X(JJACE), DUMP, CE)
C     !END
      call BYE ('CELERY')
C
      return
      end
