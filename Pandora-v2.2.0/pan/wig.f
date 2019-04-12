      subroutine WIG
     $(C,P,R,UPPER,LOWER)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Encodes a packet, for RIG.
C     (This is version 2 of WIG.)
C     !DASH
      save
C     !DASH
      real*8 C, CLIM, F, P, R, ZERO
      character LOWER*11, UPPER*11
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic abs, sign, min
C
      data CLIM /9.999/
C
      call HI ('WIG')
C     !BEG
      if(abs(C).gt.abs(P)) then
        call DIVIDE (C,R,F)
        F = sign(min(abs(F),CLIM),F)
        if(F.lt.ZERO) then
          write (UPPER,100) F
  100     format(2X,'C/R=',F5.2)
        else
          write (UPPER,101) F
  101     format(2X,'C/R=',F5.3)
        end if
        write (LOWER,102) R
  102   format(1PE11.3)
C
      else
        call DIVIDE (P,R,F)
        F = sign(min(abs(F),CLIM),F)
        write (UPPER,102) R
        if(F.lt.ZERO) then
          write (LOWER,103) F
  103     format(2X,'P/R=',F5.2)
        else
          write (LOWER,104) F
  104     format(2X,'P/R=',F5.3)
        end if
      end if
C     !END
      call BYE ('WIG')
C
      return
      end
