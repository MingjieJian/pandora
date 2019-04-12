      subroutine DULLARD
     $(X,IX,J,TE,CI)
C
C     Rudolf Loeser, 2006 Apr 14
C---- Drives DULBA to compute CI(J,TE).
C     !DASH
      save
C     !DASH
      real*8 CI, TE, X
      integer IX, J, JJLRQ, JJNLE, JJNPQ, JJXCU, JJXNU
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
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
      external DULBA, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('DULLARD')
C     !BEG
      call DULBA (J, TE, X(JJXCU), X(JJXNU), IX(JJNPQ), IX(JJLRQ),
     $            IX(JJNLE), CI)
C     !END
      call BYE ('DULLARD')
C
      return
      end
