      subroutine FEEL
     $(N,TE,NLIM)
C
C     Rudolf Loeser, 1982 May 03
C---- Computes a plot limit, for KEEL.
C     !DASH
      save
C     !DASH
      real*8 TE
      integer N, NLIM, NMIN
C     !DASH
      external HI, BYE
C
C               TE(N)
      dimension TE(*)
C
      call HI ('FEEL')
C     !BEG
      NMIN = (2*N)/3
      NLIM = N
  100 continue
        if(TE(NLIM-1).lt.TE(NLIM)) then
          NLIM = NLIM-1
          if(NLIM.gt.NMIN) then
            goto 100
          end if
        end if
      continue
C     !END
      call BYE ('FEEL')
C
      return
      end
