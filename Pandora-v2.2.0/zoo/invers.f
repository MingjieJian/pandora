      subroutine INVERS
     $(A,N,NMAX,DET,KDET,KODE,INDX,TEMP)
C     Rudolf Loeser, 1983 Jun 21
C
C---- Matrix inversion based on L\U decomposition
C     by   C R O U T 's algorithm, using routines given in
C     "NUMERICAL RECIPES" by
C
C     F l a n n e r y ,   H o r o w i t z ,   and  P r e s s .
C
C---- A, of size N x N, is the original matrix and becomes the inverse.
C     If KDET = .true., then the determinant is also computed, and is
C     returned in DET.
C     INDX (length N), and TEMP (length N x N), are scratch storage.
C
C               A(NMAX,NMAX),INDX(NMAX),TEMP(NMAX,NMAX)  Nominally, but
C               A(NMAX,N),   INDX(N),   TEMP(N,N)        is sufficient.
C
C---- Upon return, KODE = 1 indicates that everything appeared
C     to go OK; KODE = 0 indicates that a divisor was zero or that
C     overflow occurred, and that processing was not completed.
C     !DASH
      save
C     !DASH
      real*8 A, DET, ONE, TEMP, ZERO
      integer I, INDX, IPAR, J, KODE, KSIG, N, NMAX
      logical KDET
C     !DASH
      external  DECOMP, LUNDET, BAKSUB
      intrinsic max
C
      dimension A(NMAX,*), INDX(*), TEMP(N,*)
C
      data ZERO,ONE /0.D0, 1.D0/
C     !EJECT
C
C     !BEG
C---- Get L\U decomposition.
      call DECOMP   (A,N,NMAX,INDX,IPAR,KSIG,TEMP)
      if(KSIG.ne.1) then
        goto 105
      end if
C
C---- Compute determinant, if needed.
      DET = ZERO
      if(KDET) then
        call LUNDET (A,N,NMAX,IPAR,DET)
      end if
C
C---- Set up unit matrix, needed for final solution.
      do 101 J = 1,N
        do 100 I = 1,N
          TEMP(I,J) = ZERO
  100   continue
        TEMP(J,J) = ONE
  101 continue
C---- Get final solution, overwriting the unit matrix.
      do 102 J = 1,N
        call BAKSUB (A,N,NMAX,INDX,TEMP(1,J))
  102 continue
C---- Move inverse back from TEMP into A.
      do 104 J = 1,N
        do 103 I = 1,N
          A(I,J) = TEMP(I,J)
  103   continue
  104 continue
C
C---- Getting here means all is ok.
      KSIG = 1
C
C---- Go home.
  105 continue
      KODE = max(0,KSIG)
C     !END
C
      return
      end
