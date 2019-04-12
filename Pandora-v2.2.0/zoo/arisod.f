      subroutine ARISOD
     $(A,M,B,N,DELTA,KODE)
C     Rudolf Loeser, 1980 Jun 05
C---- Checks whether vectors A (length M) and B (length N)
C     are equal term-by-term, to tolerance DELTA.
C     Returns with KODE = 1 if yes, = 0 if no.
C     !DASH
      save
C     !DASH
      real*8 A, B, DELTA
      integer I, IFLG, KODE, M, N
C     !DASH
      external COMPD
C
      dimension A(M), B(N)
C
C     !BEG
      KODE = 0
      if(M.eq.N) then
        do 100 I = 1,N
          call COMPD (A(I),B(I),DELTA,IFLG)
          if(IFLG.ne.0) goto 101
  100   continue
        KODE = 1
  101   continue
      end if
C     !END
C
      return
      end
