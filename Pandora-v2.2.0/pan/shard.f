      subroutine SHARD
     $(A,N,SMP,W)
C
C     Rudolf Loeser, 1974 Oct 29
C---- Does smoothing of weights, for ORCHID.
C     !DASH
      save
C     !DASH
      real*8 A, HALF, OMS, ONE, SMP, W, X, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, HI, BYE
C
C               W(N), A(N)
      dimension W(*), A(*)
C
C
      call HI ('SHARD')
C     !BEG
      if(SMP.eq.ZERO) then
        call MOVE1 (W,N,A)
      else
C
        OMS = ONE-SMP
        do 100 I = 1,N
C
          if(I.le.1) then
            X = W(2)
          else if(I.ge.N) then
            X = W(N-1)
          else
            X = HALF*(W(I-1)+W(I+1))
          end if
C
          A(I) = OMS*W(I)+SMP*X
  100   continue
C
      end if
C     !END
      call BYE ('SHARD')
C
      return
      end
