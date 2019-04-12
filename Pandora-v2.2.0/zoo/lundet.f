      subroutine LUNDET
     $(A,N,NMAX,IPAR,DET)
C     Rudolf Loeser, 1983 Jun 21
C
C---- Computes the determinant of the N x N matrix A,
C     using IPAR and the L\U form of A as produced by DECOMP.
C
C               A(NMAX,NMAX)       Nominally, but
C               A(NMAX,N)          is sufficient.
C     !DASH
      save
C     !DASH
      real*8 A, DET
      integer I, IPAR, N, NMAX
C     !DASH
      dimension A(NMAX,*)
C
C     !BEG
      DET = IPAR
      do 100 I = 1,N
        DET = DET*A(I,I)
  100 continue
C     !END
C
      return
      end
