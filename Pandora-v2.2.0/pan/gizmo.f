      subroutine GIZMO
     $(XNE,XNE0,N,XPBL,W1,W2,XNK,SLVLS,SYM,CPR,CHI,ETA)
C
C     Rudolf Loeser, 1978 Aug 12
C---- Computes ETA values for element "SYM", for GUINEA.
C     (This is version 3 of GIZMO.)
C     !DASH
      save
C     !DASH
      real*8 CHI, CPR, ETA, SLVLS, W1, W2, XNE, XNE0, XNK, XPBL
      integer IPOP, N
      character SYM*3
C     !DASH
      external PARA, LUDMILA, MELISSA, HI, BYE
C
C               XPBL(Lenpbl), XNE(N), XNE0(N), SLVLS(N), W1(N), CPR(N),
      dimension XPBL(*),      XNE(*), XNE0(*), SLVLS(*), W1(*), CPR(*),
C
C               W2(N), XNK(N), ETA(N)
     $          W2(*), XNK(*), ETA(*)
C
      call HI ('GIZMO')
C     !BEG
C---- Determine whether "SYM" is a population ion
      call PARA      (SYM, IPOP)
      if(IPOP.gt.0) then
C----   Yes
        call LUDMILA (XPBL, IPOP, N, XNE, XNE0, W1, W2, XNK, SLVLS,
     $                SYM, CPR, CHI, ETA)
      else
C----   No
        call MELISSA (XNE, W1, W2, N, CPR, CHI, ETA)
      end if
C     !END
      call BYE ('GIZMO')
C
      return
      end
