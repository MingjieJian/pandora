      subroutine HAM
     $(ITAU,NL,KRJ,X,IX,ARHO)
C
C     Rudolf Loeser, 1974 Mar 20
C---- Establishes the set of AIJ(u,l)*RHOIJ(u,l) values at depth # ITAU,
C     for HAMMER
C     (This is version 2 of HAM.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, X
      integer ITAU, IX, JJRHO, JJYBR, KRJ, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
C     !DASH
      external ZERO1, HAG, HI, BYE
C
      dimension X(*), IX(*)
C
C               ARHO(NL*(NL-1)/2)
      dimension ARHO(*)
C
      call HI ('HAM')
C     !BEG
      call ZERO1 (ARHO,(NL*(NL-1)/2))
      call HAG   (ITAU,X(JJRHO),X(JJYBR),NL,KRJ,X,IX,ARHO)
C     !END
      call BYE ('HAM')
C
      return
      end
