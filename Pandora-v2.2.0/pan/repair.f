      subroutine REPAIR
     $(N,RAT,PRAT,IPNT)
C
C     Rudolf Loeser, 1991 Jun 05
C---- Converts Iterative Ratio into the function to be plotted
C     by IDATH.
C     (This is version 3 of REPAIR.)
C     !DASH
      save
C     !DASH
      real*8 DN, F, FL, ONE, PRAT, RAT, SIX, THREE, THSNTH, UP, ZERO
      integer I, IPNT, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 7),SIX   )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic abs
C
C               RAT(N), PRAT(N)
      dimension RAT(*), PRAT(*)
C
      data      THSNTH /1.D-3/
      data      UP, DN /0.999D0, -0.999D0/
C
      call HI ('REPAIR')
C     !BEG
      do 100 I = 1,N
        F = RAT(I)-ONE
C
        if(abs(F).le.THSNTH) then
          PRAT(I) = THREE
C
        else if(F.gt.ZERO) then
          if(F.ge.UP) then
            PRAT(I) = SIX
          else
            FL      = log10(F)
            IPNT    = IPNT+1
            PRAT(I) = SIX+FL
          end if
C
        else
          if(F.le.DN) then
            PRAT(I) = ZERO
          else
            FL      = log10(-F)
            IPNT    = IPNT+1
            PRAT(I) = -FL
          end if
        end if
C
  100 continue
C     !END
      call BYE ('REPAIR')
C
      return
      end
