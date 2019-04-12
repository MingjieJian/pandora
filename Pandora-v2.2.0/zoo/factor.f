      subroutine FACTOR
     $(R,FACT)
C     Rudolf Loeser, 1992 Jan 22
C---- Computes the factorial of R, 0 .le. R .le. 160.
C     Returns FACT=-1 for R out-of-range.
C     !DASH
      save
C     !DASH
      real*8 BIG, FACT, ONE, R, ZERO
      integer I
C     !DASH
      external FACTIN
C
      data ZERO,ONE,BIG /0.D0, 1.D0, 1.6D2/
C
C     !BEG
      if((R.lt.ZERO).or.(R.gt.BIG)) then
        FACT = -ONE
      else
        I = R
        call FACTIN (I,FACT)
      end if
C     !END
C
      return
      end
