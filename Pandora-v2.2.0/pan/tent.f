      subroutine TENT
     $(ITAU,NL,X,IX,YBR,KRJ,IU,IL,TERM)
C
C     Rudolf Loeser, 1978 Dec 03
C---- Prints CADDY-results, for GIGGLE.
C     !DASH
      save
C     !DASH
      real*8 TERM, X, YBR
      integer I, IL, ITAU, IU, IX, J, KRJ, LUEO, NL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external CADDY, LINER, PRAY, HI, BYE
C
      dimension X(*), IX(*)
C
C               TERM(NL,NL), YBR(N,NT)
      dimension TERM(NL,*),  YBR(*)
C
      call HI ('TENT')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          call CADDY (ITAU,I,J,KRJ,IU,IL,YBR,X,IX,TERM(I,J))
  100   continue
  101 continue
C
      call LINER     (1,LUEO)
      write (LUEO,102) ITAU
  102 format(' ',21X,'Depth I=',I3,5X,'(CADDY) - T(I,K,J), '
     $               'the term accompanying the upward C(I,J,K)')
      call PRAY      (LUEO,TERM,NL,NL)
C     !END
      call BYE ('TENT')
C
      return
      end
