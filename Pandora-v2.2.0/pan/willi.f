      subroutine WILLI
     $(MN1,Z,H,R,S,P,SIGMA,ZETA,Y,A,DUMP,W)
C
C     Rudolf Loeser, 1997 Aug 05
C---- Computes y (and zeta) for SWASH.
C     (This is version 2 of WILLI.)
C     !DASH
      save
C     !DASH
      real*8 A, DIJ, DZIJ, DZJ, EMDZIJ, EMZ, H, P, R, RAT, S, SIGMA, W,
     $       XP1, XP2, Y, Z, ZETA, dummy
      integer I, J, MN1
      logical DUMP
      character LABEL*42
C     !DASH
      external DIVIDE, OMEGA, QEXP2, QEXP3, WOLLO, HI, BYE
C
      dimension W(*)
C
C               J = N+MXTAP
C
C               Z(J), H(J), R(J), S(J), P(J), SIGMA(J), ZETA(J), Y(J),
      dimension Z(*), H(*), R(*), S(*), P(*), SIGMA(*), ZETA(*), Y(*),
C
C               A(J)
     $          A(*)
C
      data LABEL /'Diffusion: zeta = integral of (r/h) over Z'/
C
      call HI ('WILLI')
C     !BEG
C---- Compute sigma and p
      do 100 I = 1,MN1
        call DIVIDE  (H(I), R(I), RAT)
        SIGMA(I) = RAT*S(I)
        call DIVIDE  (R(I), H(I), P(I))
  100 continue
C---- Compute zeta
      call OMEGA     (MN1, Z, P, ZETA, A, LABEL, DUMP, W)
C---- Compute y
      Y(1) = SIGMA(1)
      do 102 I = 2,MN1
        EMZ  = exp(-ZETA(I))
        Y(I) = Y(1)*EMZ
        do 101 J = 2,I
          call WOLLO (A, I, J, DZIJ, DZJ)
          call QEXP2 (DZJ, dummy, 0, XP1)
          call QEXP3 (DZJ, dummy, 1, XP2)
          EMDZIJ = exp(-DZIJ)
          DIJ    = EMDZIJ*(SIGMA(J)*XP1+SIGMA(J-1)*XP2)
          Y(I)   = Y(I)+DIJ
  101   continue
  102 continue
C     !END
      call BYE ('WILLI')
C
      return
      end
