      subroutine LOBE
     $(N,XNK,XN1,FAC,PUL,PLU,G1)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Computes G1, for SOBEL.
C     !DASH
      save
C     !DASH
      real*8 FAC, G1, PLU, PUL, RN, XN1, XNK
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               XNK(N), XN1(N), PUL(N), PLU(N), G1(N), FAC(N)
      dimension XNK(*), XN1(*), PUL(*), PLU(*), G1(*), FAC(*)
C
      call HI ('LOBE')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (XNK(I),XN1(I),RN)
        G1(I) = PUL(I)*(RN*FAC(I))-PLU(I)
  100 continue
C     !END
      call BYE ('LOBE')
C
      return
      end
