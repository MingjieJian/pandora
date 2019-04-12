      subroutine SCALMAT
     $(XORIG,N,NMAX,SKALEC,SKALER,XSCLD)
C     Rudolf Loeser, 1983 Dec 20
C---- Applies column and row scale factors to the original
C     matrix XORIG(N,N), to produce the scaled matrix
C     XSCLD(N,N), in the hope that the scaled matrix is
C     better suited for inversion.
C---- The inverse computed from the scaled matrix can then
C     be rescaled by subroutine "SCALINV", so that it will then
C     be the inverse of XORIG.
C     !DASH
      save
C     !DASH
      real*8 SKALEC, SKALER, XORIG, XSCLD
      integer N, NMAX
C     !DASH
      external  SCALIT
C
      dimension XORIG(NMAX,N), XSCLD(NMAX,N), SKALEC(N), SKALER(N)
C
C     !BEG
      call SCALIT (XORIG,N,NMAX,SKALEC,SKALER,XSCLD)
C     !END
C
      return
      end
