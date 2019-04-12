      subroutine DPRAY
     $(LU,A,N,M)
C
C     Rudolf Loeser, 2002 Dec 17
C---- Prints an array of size N x M, 16 significant figures.
C     (This is version 2 of DPRAY.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer LU, M, N
C     !DASH
      external LINER, RAUMA
C
      dimension A(N,M)
C
C     !BEG
      if((LU.gt.0).and.(N.gt.0).and.(M.gt.0)) then
        call LINER (1,LU)
        call RAUMA (LU,A,N,M,2)
      end if
C     !END
C
      return
      end
