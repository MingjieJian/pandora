      subroutine PRIVET
     $(LU,V,N)
C
C     Rudolf Loeser, 2002 Dec 04
C---- Prints a vector of length N, 4 significant figures.
C     (This is version 2 of PRIVET.)
C     !DASH
      save
C     !DASH
      real*8 V
      integer LU, N
C     !DASH
      external LINER, VICAR
C
      dimension V(N)
C
C     !BEG
      if((LU.gt.0).and.(N.gt.0)) then
        call LINER (1,LU)
        call VICAR (LU,V,N,1)
      end if
C     !END
C
      return
      end
