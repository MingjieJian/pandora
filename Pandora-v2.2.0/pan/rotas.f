      subroutine ROTAS
     $(A,JB,JE,NSL,TER,I,NTE,ARR,K)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Selects array members to be printed, for TOAD.
C     (This is version 2 of ROTAS.)
C     !DASH
      save
C     !DASH
      real*8 A, ARR, TER
      integer I, J, JB, JE, K, NSL, NTE
C     !DASH
      external ZERO1, HI, BYE
C
C               A(NTE,NSL), TER(NTE), ARR(8)
      dimension A(NTE,*),   TER(*),   ARR(*)
C
      call HI ('ROTAS')
C     !BEG
      call ZERO1 (ARR, 8)
      K = 1
C
      if(NTE.gt.0) then
        ARR(K) = TER(I)
      end if
C
      do 100 J = JB,JE
        K = K+1
        ARR(K) = A(I,J)
  100 continue
C     !END
      call BYE ('ROTAS')
C
      return
      end
