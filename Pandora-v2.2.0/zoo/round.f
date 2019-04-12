      subroutine ROUND
     $(F,N,CRIT,KNT)
C
C     Rudolf Loeser, 2001 Nov 29
C---- Rounds the values of F to the nearest exact multiple of CRIT.
C     CRIT must be > 0. Note: CRIT = 1 is a special case.
C     Upon return, the rounded values have replaced the input values
C     in F, and KNT tells how many values were changed.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, G, ONE, ZERO
      integer I, KNT, N
      logical RONE
C     !DASH
      external  ROUND1, ROUNDX, ABORT
      intrinsic aint
C
      dimension F(N)
C
      data ZERO,ONE /0.D0, 1.D0/
C
      call HI ('ROUND')
C     !BEG
      KNT = 0
      if(N.gt.0) then
C
        if(CRIT.le.ZERO) then
          write (*,100) CRIT
  100     format(' ','Error in ROUND:  CRIT =',1PE24.16,
     $               ', which is unacceptable.')
          call ABORT
        end if
C
        RONE = CRIT.eq.ONE
C
        do 101 I = 1,N
          if(RONE) then
            call ROUND1 (F(I),G)
          else
            call ROUNDX (F(I),CRIT,G)
          end if
          if(G.ne.F(I)) then
            F(I) = G
            KNT  = KNT+1
          end if
  101   continue
C
      end if
C     !END
      call BYE ('ROUND')
C
      return
      end
