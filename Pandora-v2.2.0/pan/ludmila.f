      subroutine LUDMILA
     $(XPBL,IPOP,N,XNE,XNE0,W1,W2,XNK,SLVLS,SYM,CPR,CHI,ETA)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Controls computation of non-LTE ETA values for Population Ion
C     "IPOP", for GIZMO.
C     (This is version 2 of LUDMILA.)
C     !DASH
      save
C     !DASH
      real*8 CHI, CPR, ETA, SLVLS, W1, W2, XNE, XNE0, XNK, XPBL, dummy
      integer IPOP, N
      character SYM*3
C     !DASH
      external POPUTIL, VASEY, OLGA, HI, BYE
C
C               XPBL(Lenpbl), XNE(N), XNE0(N), W1(N), CPR(N), SLVLS(N),
      dimension XPBL(*),      XNE(*), XNE0(*), W1(*), CPR(*), SLVLS(*),
C
C               W2(N), ETA(N), XNK(N)
     $          W2(*), ETA(*), XNK(*)
C
      call HI ('LUDMILA')
C     !BEG
      call POPUTIL (XPBL, IPOP, 0, dummy, 1, XNK, 1, SLVLS, 0, dummy)
      call VASEY   (N, XNK, SLVLS, ETA)
      call OLGA    (XNE, XNE0, W1, W2, N, SYM, CPR, CHI, ETA)
C     !END
      call BYE ('LUDMILA')
C
      return
      end
