      subroutine KURNAI
     $(M,KINOUT,I4DIO,I4DFM,I4DEQ,Z,F,G,R,S,A,B,C,D,Y,CHK,W,IW,DUMP)
C
C     Rudolf Loeser, 1997 Oct 22
C---- Solves the fourdiagonal diffusion equations for y.
C     (This is version 2 of KURNAI.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, F, G, R, S, W, Y, Z
      integer I4DEQ, I4DFM, I4DIO, IDEL, IDELB, IETA, IFB, IGB, IN,
     $        INOUT, IRB, IRHO, IS, ISB, ISIGMA, IW, IYA, IYO, IZETA,
     $        KINOUT, M, MOX
      logical DUMP
C     !DASH
      external LULU, URKINA, AGNES, IGNIS, URIAL, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               Z(J), F(J), G(J), R(J), CHK(J), A(J), B(J), C(J), D(J),
      dimension Z(*), F(*), G(*), R(*), CHK(*), A(*), B(*), C(*), D(*),
C
C               Y(J), S(J)
     $          Y(*), S(*)
C
      dimension IN(12)
      equivalence
     $(IN( 1),IDEL  ),(IN( 2),IFB   ),(IN( 3),IGB   ),(IN( 4),IRB   ),
     $(IN( 5),ISB   ),(IN( 6),IDELB ),(IN( 7),IZETA ),(IN( 8),IETA  ),
     $(IN( 9),ISIGMA),(IN(10),IRHO  ),(IN(11),IYO   ),(IN(12),IYA   )
C
      call HI ('KURNAI')
C     !BEG
C     (Get, and allocate, W allotment)
      call LULU   (IN, IS, MOX, 'KURNAI', M)
C
      call URKINA (KINOUT, I4DIO, INOUT)
C---- "Original" solution (functions of Z)
      call AGNES  (M, INOUT, I4DEQ, Z, F, G, R, S, A, B, C, D,
     $             W(IDEL), W(IFB), W(IGB), W(IRB), W(ISB), W(IDELB),
     $             W(IYO), CHK, W, IW, DUMP)
C---- "Alternate" solution (functions of zeta)
      call IGNIS  (M, INOUT, I4DEQ, Z, F, G, R, S, A, B, C, D,
     $             W(IDEL), W(IFB), W(IGB), W(IRB), W(ISB), W(IDELB),
     $             W(IZETA), W(IETA), W(ISIGMA), W(IRHO),
     $             W(IYA), CHK, W, IW, DUMP)
C---- Choose one, and print comparison
      call URIAL  (DUMP, W(IYO), W(IYA), Y, I4DFM, M)
C
C     (Give back W allotment)
      call WGIVE  (W, 'KURNAI')
C     !END
      call BYE ('KURNAI')
C
      return
      end
