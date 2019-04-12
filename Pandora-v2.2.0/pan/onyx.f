      subroutine ONYX
     $(XLAM,DL,WID,YNT,YNTI)
C
C     Rudolf Loeser, 1977 Jan 26
C---- Computes integrated emergent quantity,
C     where quantity is either Intensity /Hz or Flux /Hz.
C     !DASH
      save
C     !DASH
      real*8 CON, DL, RAT, TWO, WID, XLAM, YNT, YNTI, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  RIGEL, DIVIDE, HI, BYE
      intrinsic abs
C
      call HI ('ONYX')
C     !BEG
      if((YNT.le.ZERO).or.(WID.eq.ZERO)) then
        YNTI = ZERO
      else
        call RIGEL  (28,CON)
        call DIVIDE (CON,(XLAM**2),RAT)
        YNTI = RAT*YNT*(TWO*abs(DL)-WID)
      end if
C     !END
      call BYE ('ONYX')
C
      return
      end
