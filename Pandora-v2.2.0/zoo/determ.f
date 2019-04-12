      subroutine DETERM
     $(A,N,NMAX,DET,KODE,INDX,VV)
C     Rudolf Loeser, 1990 Dec 11
C
C---- Determinant calculation based on L\U decomposition
C     by   C R O U T 's algorithm, using routines given in
C     "NUMERICAL RECIPES" by
C
C     F l a n n e r y ,   H o r o w i t z   and   P r e s s .
C
C---- A, of size N x N, is stored in the FORTRAN manner
C     (i.e. comun-wise), with column dimension NMAX.
C     Its contents are destroyed by DETERM.
C     The determinant is returned in DET.
C     INDX and VV, each of length N, are scratch storage.
C
C---- Upon return, KODE = 1 indicates that everything appeared
C     to go ok; KODE = 0 indicates that a divisor was zero or that
C     overflow occurred, and that processing was not completed.
C     !DASH
      save
C     !DASH
      real*8 A, DET, VV, ZERO
      integer INDX, IPAR, KODE, KSIG, N, NMAX
C     !DASH
      external  DECOMP, LUNDET
      intrinsic max
C
C               A(NMAX,NMAX), INDX(NMAX), VV(NMAX)     Nominally, but
C               A(NMAX,N)     INDX(N),    VV(N)        is sufficient.
      dimension A(NMAX,*),    INDX(*),    VV(*)
C
      data ZERO /0.D0/
C
C     !BEG
      DET = ZERO
C---- Get L\U decomposition
      call DECOMP (A,N,NMAX,INDX,IPAR,KSIG,VV)
      if(KSIG.ne.1) goto 100
C---- Compute determinant
      call LUNDET (A,N,NMAX,IPAR,DET)
C---- Getting here means all is ok.
      KSIG = 1
C---- Go home
  100 continue
      KODE = max(0,KSIG)
C     !END
C
      return
      end
