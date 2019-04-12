      subroutine EFE
     $(EQNL,EQNU,W,D)
C
C     Rudolf Loeser, 1991 May 15
C---- Computes intermediates for DERVENI.
C     !DASH
      save
C     !DASH
      real*8 D, EQNL, EQNU, ONE, RN, RNL, RNU, W
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('EFE')
C     !BEG
      call DIVIDE (EQNL, EQNU, RN)
      call DIVIDE (ONE,  EQNL, RNL)
      call DIVIDE (ONE,  EQNU, RNU)
C
      W = RNL**2-RNU**2
      D = (RN**2)*(W**4)
C     !END
      call BYE ('EFE')
C
      return
      end
