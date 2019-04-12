      subroutine ROUND1
     $(F,G)
C
C     Rudolf Loeser, 2001 Nov 29
C---- Rounds the value of F to the nearest integer.
C     The result is returned in G.
C     !DASH
      save
C     !DASH
      real*8 D, F, G, HALF, ONE, ZERO
C     !DASH
      intrinsic aint
C
      data ZERO,HALF,ONE /0.D0, 5.D-1, 1.D0/
C
      call HI ('ROUND1')
C     !BEG
      if(F.ne.ZERO) then
        G = aint(F)
        D = F-G
        if(F.gt.ZERO) then
          if((+D).gt.HALF) then
            G = G+ONE
          end if
        else
          if((-D).gt.HALF) then
            G = G-ONE
          end if
        end if
      else
        G = ZERO
      end if
C     !END
      call BYE ('ROUND1')
C
      return
      end
