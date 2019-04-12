      subroutine PEONY
     $(IU,IL,N,BDS,ALF,BATA,S)
C
C     Rudolf Loeser, 1976 Jun 10
C---- Computes old-style BD ratios.
C     (This is version 2 of PEONY.)
C     !DASH
      save
C     !DASH
      real*8 A, ALF, BATA, BDS, DIV, ONE, R, S
      integer I, IL, IU, IUL, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external INDXUL, DIVIDE, HI, BYE
C
C               BDS(N), ALF(MUL), BATA(N,MUL), S(N)
      dimension BDS(*), ALF(*),   BATA(N,*),   S(*)
C
      call HI ('PEONY')
C     !BEG
      call INDXUL (IU,IL,IUL)
      A = ALF(IUL)
      do 100 I = 1,N
        call DIVIDE (A,S(I),R)
        DIV = BATA(I,IUL)*(ONE+R)
        call DIVIDE (ONE,DIV,BDS(I))
  100 continue
C     !END
      call BYE ('PEONY')
C
      return
      end
