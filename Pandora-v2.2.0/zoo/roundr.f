      subroutine ROUNDR
     $(F,M,G)
C
C     Rudolf Loeser, 2001 Nov 30
C---- Rounds the value of F to M figures.
C     The result is returned in G.
C     !DASH
      save
C     !DASH
      real*8 CRIT, D, F, FL, G, HALF, ONE, R, TEN, X, ZERO
      integer I, M
C     !DASH
      intrinsic aint, abs
C
      data ZERO,HALF,ONE,TEN /0.D0, 5.D-1, 1.D0, 1.D1/
C
      call HI ('ROUNDR')
C     !BEG
      if(F.ne.ZERO) then
        FL = log10(abs(F))
        I  = FL
        if(I.ge.0) then
          I = I-M+1
        else
          I = I+M
        end if
        CRIT = TEN**I
C
        R = F/CRIT
        X = aint(R)
        D = R-X
        if(R.gt.ZERO) then
          if((+D).gt.HALF) then
            X = X+ONE
          end if
        else
          if((-D).gt.HALF) then
            X = X-ONE
          end if
        end if
        G = X*CRIT
      else
        G = ZERO
      end if
C     !END
      call BYE ('ROUNDR')
C
      return
      end
