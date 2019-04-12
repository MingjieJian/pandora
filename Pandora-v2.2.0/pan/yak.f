      subroutine YAK
     $(N,YK,RLI)
C
C     Rudolf Loeser, 1978 Jan 26
C---- Modifies RL for additional recombinations.
C     !DASH
      save
C     !DASH
      real*8 F, ONE, RLI, YK
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external CONMUL, HI, BYE
C
C               RLI(N)
      dimension RLI(*)
C
      call HI ('YAK')
C     !BEG
      F = ONE+YK
      call CONMUL (F,RLI,N)
C     !END
      call BYE ('YAK')
C
      return
      end
