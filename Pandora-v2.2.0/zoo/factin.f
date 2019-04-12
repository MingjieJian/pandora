      subroutine FACTIN
     $(I,FACT)
C     Rudolf Loeser, 1992 Jan 22
C---- Computes the factorial of I, 0 .le. I .le. 160.
C     Returns FACT=-1 for I out-of-range.
C     !DASH
      save
C     !DASH
      real*8 F, FACT, FCT, ONE
      integer I, J
C     !DASH
      dimension FCT(161)
C
      data      ONE,FCT(1),FCT(2),FCT(3) /4*1.D0/
C
C     !BEG
      if((I.lt.0).or.(I.gt.160)) then
        FACT = -ONE
      else
        if(FCT(3).eq.ONE) then
          do 100 J = 3,161
            F = J-1
            FCT(J) = F*FCT(J-1)
  100     continue
        end if
        FACT = FCT(I+1)
      end if
C     !END
C
      return
      end
