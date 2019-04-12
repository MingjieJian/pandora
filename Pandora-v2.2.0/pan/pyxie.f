      subroutine PYXIE
     $(FRR,MRR,Z,OPAC,S,N,DX,FX,GX,TX,EMI,WVL,TIT,W,DUMP)
C
C     Rudolf Loeser, 1981 Jul 01
C---- Computes disk intensities, for RAJA.
C     !DASH
      save
C     !DASH
      real*8 DX, EMI, FRR, FX, GX, OPAC, S, TX, W, WVL, Z
      integer I, IRT, MRR, N, jummy
      logical DMPI, DUMP, lummy
      character LABEL*127, TIT*(*)
C     !DASH
      external DAPHNIS, FINK, HI, BYE
C
      dimension W(*)
C
C               DX(NRPMX), FX(NRPMX), GX(NRPMX), TX(NRPMX), Z(N), S(N),
      dimension DX(*),     FX(*),     GX(*),     TX(*),     Z(*), S(*),
C
C               EMI(MRR), FRR(MRR), OPAC(N)
     $          EMI(*),   FRR(*),   OPAC(*)
C
      call HI ('PYXIE')
C     !BEG
      IRT = MRR/2
C
      do 100 I = 1,MRR
        call DAPHNIS (DUMP, 1, I, IRT, WVL, jummy, TIT, lummy, LABEL,
     $                DMPI)
C
        call FINK    (N, Z, OPAC, S, DX, FX, GX, TX, FRR(I), DMPI, I,
     $                EMI(I), LABEL, W)
  100 continue
C     !END
      call BYE ('PYXIE')
C
      return
      end
