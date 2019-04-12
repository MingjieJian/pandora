      subroutine MASAI
     $(N,F,P,HNDS,HNDP,HND,CRIT,CONVERG)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Computes a new trial HND table, and checks for convergence,
C     for H.S.E.
C     (This is version 2 of MASAI.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, HND, HNDP, HNDS, P, dummy
      integer I, N, jummy
      logical CONVERG
C     !DASH
      external  MOVE1, ARRDIV, CONVERD, WENDY, HI, BYE
      intrinsic abs
C
C               P(N), F(N), HNDS(N), HNDP(N), HND(N)
      dimension P(*), F(*), HNDS(*), HNDP(*), HND(*)
C
      call HI ('MASAI')
C     !BEG
C---- Save previous values
      call MOVE1   (HND, N, HNDS)
C
C---- Compute
      call ARRDIV  (P, F, HNDP, N)
      do 100 I = 1,N
        HND(I) = sqrt(abs(HNDP(I)*HNDS(I)))
  100 continue
C
C---- Check for convergence
      call CONVERD (HNDS, 1, N, HND, 1, N, CRIT, dummy, jummy,
     $              CONVERG)
C---- Continuum Recalculation control
      call WENDY   (HND, 1, N, 4, 'MASAI')
C     !END
      call BYE ('MASAI')
C
      return
      end
