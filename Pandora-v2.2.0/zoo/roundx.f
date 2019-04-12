      subroutine ROUNDX
     $(F,CRIT,G)
C
C     Rudolf Loeser, 2001 Nov 29
C---- Rounds the value of F to the nearest exact multiple of CRIT.
C     The user must insure that CRIT > 0.
C
C     The result is returned in G.
C     !DASH
      save
C     !DASH
      real*8 CRIT, D, F, G, HALF, ONE, R, X, ZERO
C     !DASH
      intrinsic aint
C
      data ZERO,HALF,ONE /0.D0, 5.D-1, 1.D0/
C
      call HI ('ROUNDX')
C     !BEG
      if(F.ne.ZERO) then
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
      call BYE ('ROUNDX')
C
      return
      end
