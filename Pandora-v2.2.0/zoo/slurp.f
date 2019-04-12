      subroutine SLURP
     $(D,F,I, P)
C     Rudolf Loeser, 1997 Feb 07
C     !DASH
      save
C     !DASH
      real*8 D, DF0, DFL, F, F0, FL, FM, FP, ONE, P
      integer I
C     !DASH
      intrinsic sign, abs
      dimension D(*), F(*)
C
      data ONE /1.D0/
C
C     !BEG
      P = F(I)
C
      FM = F(I-1)+(F(I-1)-F(I-2))*(D(I)/D(I-1))
      FP = F(I+1)-(F(I+2)-F(I+1))*(D(I+1)/D(I+2))
      FL = (F(I-1)*D(I+1)+F(I+1)*D(I))/(D(I)+D(I+1))
C
      if(sign(ONE,(FP-FL)).eq.sign(ONE,(FM-FL))) then
        if((abs(FM-FL)).lt.(abs(FP-FL))) then
          F0 = FM
        else
          F0 = FP
        end if
        DFL = P-FL
        DF0 = P-F0
        if(sign(ONE,DFL).eq.sign(ONE,DF0)) then
          if(abs(DFL).lt.abs(DF0)) then
            P = FL
          else
            P = F0
          end if
        end if
      else
        P = FL
      end if
C     !END
C
      return
      end
