      subroutine ELCMAR
     $(XL,WL,WU,XR)
C
C     Rudolf Loeser, 1991 Mar 28
C---- Tries to constrain abscissa limits, for DANCE.
C     !DASH
      save
C     !DASH
      real*8 D, RWL, RWU, RXL, RXR, TEN, TENTH, WL, WU, XL, XR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(19),TENTH )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external HI, BYE
C
      call HI ('ELCMAR')
C     !BEG
      RXL = TEN**XL
      RWL = TEN**WL
      RWU = TEN**WU
      RXR = TEN**XR
      if(((RWU-RWL)/(RXR-RXL)).le.TENTH) then
        XL = WL
        XR = WU
      else
        D = RXL
  100   continue
          if((RXL+D).le.RWL) then
            RXL = RXL+D
            go to 100
          end if
  101   continue
          if((RXR-D).ge.RWU) then
            RXR = RXR-D
            go to 101
          end if
        XL = log10(RXL)
        XR = log10(RXR)
      end if
C     !END
      call BYE ('ELCMAR')
C
      return
      end
