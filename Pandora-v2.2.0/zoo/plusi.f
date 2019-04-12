      subroutine PLUSI
     $(A,INC,N,LGT)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, IMAX, IMIN, INC, K, LGT, N, ZERO
C     !DASH
      external SCANI
C
      dimension A(*)
C
      data ZERO /0/
C
C     !BEG
      call SCANI (A,INC,N,ZERO,ZERO,ZERO,IMIN,IMAX,K,K,LGT,K)
C     !END
C
      return
      end
