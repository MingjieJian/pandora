      subroutine PXPNT2
     $(NM1,X,EI,FNM1,ERROR)
C     Rudolf Loeser, 1988 Nov 21
C---- Computes the series expansion, for PEXPINT.
C     !DASH
      save
C     !DASH
      real*8 ANS, DEL, EI, EPS, F, FD, FI, FII, FNM1, GAMMA, ONE, PSI,
     $       X, XLOG
      integer I, II, ITMAX, NM1
      logical ERROR
C     !DASH
      intrinsic abs
C
      data ITMAX /100/
      data EPS /1.D-14/
C     EPS is the desired accuracy, but should not be smaller than
C     the machine precision.
      data GAMMA /0.5772156649015328606D0/
      data ONE /1.D0/
C
C     !BEG
      XLOG = log(X)
      if(NM1.ne.0) then
        ANS = ONE/FNM1
      else
        ANS = -XLOG-GAMMA
      end if
      F = ONE
      do 101 I = 1,ITMAX
        FI =  I
        F  = -F*X/FI
        if(I.ne.NM1) then
          FD  =  I-NM1
          DEL = -F/FD
        else
          PSI = -GAMMA
          do 100 II = 1,NM1
            FII = II
            PSI = PSI+ONE/FII
  100     continue
          DEL = F*(-XLOG+PSI)
        end if
        ANS  = ANS+DEL
        if(abs(DEL).lt.abs(ANS*EPS)) then
          goto 102
        end if
  101 continue
      ERROR = .true.
  102 continue
      EI = ANS
C     !END
C
      return
      end
