      subroutine BRINK
     $(X,XC,XP,DRLIM,DR)
C
C     Rudolf Loeser, 1988 Jul 20
C---- Computes DR, for DEAR.
C     !DASH
      save
C     !DASH
      real*8 DR, DRLIM, DX, ONE, X, XC, XP, Z, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic max
C
      call HI ('BRINK')
C     !BEG
      DX = X-XC
      if(DX.le.ZERO) then
        DR = ONE
      else
C
        call DIVIDE (DX,XC,Z)
        Z  = Z**XP
        DR = exp(-Z)
        DR = max(DR,DRLIM)
      end if
C     !END
      call BYE ('BRINK')
C
      return
      end
