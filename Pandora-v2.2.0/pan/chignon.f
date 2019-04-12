      subroutine CHIGNON
     $(X,IX,J,TE,DENS,DUMP,CI)
C
C     Rudolf Loeser, 2006 Apr 12
C---- Drives CHINON to compute CI(J,TE,DENS).
C     !DASH
      save
C     !DASH
      real*8 CI, DENS, TE, X
      integer IX, J, JJACI, JJCII, JJLRQ, JJMCI, JJNLE, JJNPQ, JJTER,
     $        JJXCU, JJXNU, NTE
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(20),NTE)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ(204),JJACI)
      equivalence (IZOQ(260),JJXCU)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
      equivalence (JZOQ( 12),JJNLE)
C     !DASH
      external CHINON, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('CHIGNON')
C     !BEG
      call CHINON (J, NTE, X(JJTER), X(JJCII), TE, DENS, X(JJXNU),
     $             X(JJXCU), IX(JJNPQ), IX(JJLRQ), IX(JJNLE),
     $             X(JJMCI), X(JJACI), DUMP, CI)
C     !END
      call BYE ('CHIGNON')
C
      return
      end
