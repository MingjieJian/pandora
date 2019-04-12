      subroutine ELM
     $(TAU,T,N)
C
C     Rudolf Loeser, 2000 Jan 25
C---- Sets up the extended T table, for RT weight matrix calculation.
C     (This is version 3 of ELM.)
C     !DASH
      save
C     !DASH
      real*8 T, TAU, TT, TWO, ZERO
      integer I, IN, N, N2
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               TAU(N), T(2*N)
      dimension TAU(*), T(*)
C
      call HI ('ELM')
C     !BEG
      N2 = 2*N
      TT = TWO*TAU(N)
      IN = N
      do 100 I = 1,N2
        if(I.le.N) then
          T(I) = TAU(I)
        else
          IN   = IN-1
          T(I) = TT-TAU(IN)
        end if
  100 continue
C     !END
      call BYE ('ELM')
C
      return
      end
