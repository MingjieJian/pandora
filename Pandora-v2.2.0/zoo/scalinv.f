      subroutine SCALINV
     $(XINVO,N,NMAX,SKALEC,SKALER,XINVS)
C     Rudolf Loeser, 1983 Dec 20
C---- Applies column and row scale factors to the original
C     inverse matrix XINVO(N,N), to produce the rescaled
C     inverse XINVS(N,N).
C---- XINVO is the inverse of a matrix that had been scaled
C     prior to inversion by subroutine "SCALMAT".
C     !DASH
      save
C     !DASH
      real*8 SKALEC, SKALER, XINVO, XINVS
      integer N, NMAX
C     !DASH
      external  SCALIT
C
      dimension XINVO(NMAX,N), XINVS(NMAX,N), SKALEC(N), SKALER(N)
C
C     !BEG
      call SCALIT (XINVO,N,NMAX,SKALER,SKALEC,XINVS)
C     !END
C
      return
      end
