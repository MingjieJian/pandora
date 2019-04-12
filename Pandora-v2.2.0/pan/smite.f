      subroutine SMITE
     $(SA,CHKA,RCHK)
C
C     Rudolf Loeser, 2004 Feb 10
C---- Computes RCHK, for MUTISM.
C     !DASH
      save
C     !DASH
      real*8 CHKA, RCHK, SA, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic abs
C
      call HI ('SMITE')
C     !BEG
      if(SA.ne.ZERO) then
        RCHK = abs((CHKA-SA)/SA)
      else
        RCHK = abs(CHKA)
      end if
C     !END
      call BYE ('SMITE')
C
      return
      end
