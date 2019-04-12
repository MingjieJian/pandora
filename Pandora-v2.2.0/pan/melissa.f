      subroutine MELISSA
     $(XNE,W1,W2,N,CPR,CHI,ETA)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Computes a table of LTE ETA values, for GIZMO.
C     (This is version 2 of MELISSA.)
C     !DASH
      save
C     !DASH
      real*8 CHI, CPR, ETA, W1, W2, XNE
      integer I, N
C     !DASH
      external TAMARA, HI, BYE
C
C               ETA(N), CPR(N), XNE(N), W1(N), W2(N)
      dimension ETA(*), CPR(*), XNE(*), W1(*), W2(*)
C
      call HI ('MELISSA')
C     !BEG
      do 100 I = 1,N
        call TAMARA (XNE(I), W1(I), W2(I), CPR(I), CHI, ETA(I))
  100 continue
C     !END
      call BYE ('MELISSA')
C
      return
      end
