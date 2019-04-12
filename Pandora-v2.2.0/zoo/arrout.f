      subroutine ARROUT
     $(LU,A,N,M,LABEL)
C
C     Rudolf Loeser, 2002 Dec 17
C---- Prints a labelled array of size N x M, 4 significant figures.
C     (This is version 2 of ARROUT.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer LU, M, N
      character LABEL*(*)
C     !DASH
      external LINER, RAUMA
C
      dimension A(N,M)
C
C     !BEG
      if((LU.gt.0).and.(N.gt.0).and.(M.gt.0)) then
        call LINER (2,LU)
        write (LU,100) LABEL
  100   format(' ',A)
C
        call LINER (1,LU)
        call RAUMA (LU,A,N,M,1)
      end if
C     !END
C
      return
      end
