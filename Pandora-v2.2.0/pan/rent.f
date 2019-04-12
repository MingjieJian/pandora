      subroutine RENT
     $(ITAU,NL,X,IX,RHO,YBR,KRJ,TERM)
C
C     Rudolf Loeser, 1978 Dec 03
C---- Prints ARROW-results, for GIGGLE.
C     !DASH
      save
C     !DASH
      real*8 RHO, TERM, X, YBR
      integer I, ITAU, IX, J, KRJ, LUEO, NL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ARROW, LINER, PRAY, HI, BYE
C
      dimension X(*), IX(*)
C
C               TERM(NL,NL), RHO(N,NT), YBR(N,NT)
      dimension TERM(NL,*),  RHO(*),    YBR(*)
C
      call HI ('RENT')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          call ARROW (ITAU,I,J,KRJ,RHO,YBR,X,IX,TERM(I,J))
  100   continue
  101 continue
C
      call LINER     (1,LUEO)
      if(KRJ.eq.3) then
        write (LUEO,102) ITAU, '-prime,'
  102   format(' ','******************** Depth I=',I3,5X,
     $         '(ARROW) - A(J,K)*RHO(I,J,K)',A,' or its surrogate')
      else
        write (LUEO,102) ITAU, ','
      end if
      call PRAY      (LUEO,TERM,NL,NL)
C     !END
      call BYE ('RENT')
C
      return
      end
