      subroutine ORNIS
     $(L,X,CQL,XCQL,TEMP,Y,Z)
C
C     Rudolf Loeser, 1990 Oct 11
C---- Computes parameters pertaining to the theoretical
C     Hydrogen atomic model.
C
C     Johnson, L.C. 1972, ApJ, 174, 227-236.
C
C     !DASH
      save
C     !DASH
      real*8 CON58, CQL, EL2, ELL, ELP, TEMP, X, XCQL, Y, Z, Z1, Z2, Z3
      integer L
C     !DASH
      external RIGEL, HI, BYE
C
      data Z1,Z2,Z3 /1.94D0, -1.57D0, 4.5D-1/
C
      call HI ('ORNIS')
C     !BEG
      ELL = L
      EL2 = L**2
      ELP = ELL**Z2
C
      call RIGEL (58, CON58)
      Y = (X/(EL2*CQL))*(CON58/TEMP)
C
      if(L.eq.1) then
        Z = Y+XCQL*Z3
      else
        Z = Y+XCQL*(Z1*ELP)
      end if
C     !END
      call BYE ('ORNIS')
C
      return
      end
