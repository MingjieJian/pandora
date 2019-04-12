      subroutine ABSVECD
     $(A,INCA,NA,B,INCB,NB)
C     Rudolf Loeser, 1984 Dec 11
C---- Sets the members of array B equal to
C     the absolute values of the members of array A.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer I, INA, INB, INCA, INCB, M, NA, NB
C     !DASH
      intrinsic abs, min
C
      dimension A(*), B(*)
C
C     !BEG
      M = min(NA,NB)
      if(M.gt.0) then
        INA = 1-INCA
        INB = 1-INCB
        do 100 I=1,M
          INA = INA+INCA
          INB = INB+INCB
          B(INB) = abs(A(INA))
  100   continue
      end if
C     !END
C
      return
      end
