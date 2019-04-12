      subroutine IRRSUM
     $(IA,N,ISUM)
C     Rudolf Loeser, 1995 Jul 19
C---- Computes the sum of the elements of an integer array.
C     (A version of ARRSUM)
C     !DASH
      save
C     !DASH
      integer I, IA, ISUM, N
C     !DASH
      dimension IA(N)
C
C     !BEG
      ISUM = 0
      if(N.gt.0) then
        do 100 I = 1,N
          ISUM = ISUM+IA(I)
  100   continue
      end if
C     !END
C
      return
      end
