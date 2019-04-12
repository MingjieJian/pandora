      subroutine VECOUT
     $(LU,V,N,LABEL)
C
C     Rudolf Loeser, 2002 Dec 04
C---- Prints a labelled vector of length N, 4 significant figures.
C     (This is version 2 of VECOUT.)
C     !DASH
      save
C     !DASH
      real*8 V
      integer LU, N
      character LABEL*(*)
C     !DASH
      external LINER, VICAR
C
      dimension V(N)
C
C     !BEG
      if((LU.gt.0).and.(N.gt.0)) then
        call LINER (2,LU)
        write (LU,100) LABEL
  100   format(' ',A)
C
        call LINER (1,LU)
        call VICAR (LU,V,N,1)
      end if
C     !END
C
      return
      end
