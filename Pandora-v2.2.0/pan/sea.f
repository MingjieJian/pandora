      subroutine SEA
     $(XLAM,FLUX,BT)
C
C     Rudolf Loeser, 1974 Feb 07
C---- Computes brightness temperature, from FLUX.
C     !DASH
      save
C     !DASH
      real*8 A2, BT, CON9, CWARTR, FLUX, HALF, ONE, THIRD, X, XLAM, Y,
     $       Z, ZERO, ZX1, ZX2
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, RIGEL, HI, BYE
C
      data ZX1, ZX2 /1.D-4, 1.D-2/
      data A2 /8.0988762D-1/
C
      call HI ('SEA')
C     !BEG
      if(FLUX.le.ZERO) then
        BT = ZERO
      else
C
        X = ZX1*XLAM
        call DIVIDE (A2, ((X**5)*FLUX), Z)
        if(Z.ge.ZX2) then
          Y = log(ONE+Z)
        else
          Y = Z*(ONE-Z*(HALF-Z*(THIRD-Z*CWARTR)))
        end if
        call RIGEL  (9, CON9)
        call DIVIDE (CON9, (X*Y), BT)
C
      end if
C     !END
      call BYE ('SEA')
C
      return
      end
