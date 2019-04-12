      subroutine DRAYSON
     $(X,Y,VOIGT)
C     Rudolf Loeser, 1990 Dec 05
C
C---- Algorithm by   S .   R .   D r a y s o n
C
C     to compute the Voigt profile for all x,y .ge. 0.
C
C     See:
C     Drayson, S.R., Rapid Computation of the Voigt Profile
C       (1976), JQSRT, Vol. 16, pp. 611-614
C     Rees, P.C.T., Some Voigt Function Bench-marks
C       (1990), Newsletter on Analysis of Astronomical Spectra
C       Daresbury Laboratory (UK), No. 14 (September 1990)
C     !DASH
      save
C     !DASH
      real*4 C1, C2, CRIT, FIVE, ONE, ROOTPI, VAL, VOIGT, X, Y
C     !DASH
      external EDAWSON, EFRACT, EGAUSH
C
      data ROOTPI / 1.7724539E0 /
      data C1, C2 /1.85E0, 3.6E0/
      data ONE, FIVE /1.E0, 5.E0/
C
C     !BEG
      if(X.ge.FIVE) then
        call EGAUSH      (X,Y,0,VAL)
      else
        if(Y.le.ONE) then
          if((X+Y).ge.FIVE) then
            call EGAUSH  (X,Y,1,VAL)
          else
            call EDAWSON (X,Y,VAL)
          end if
        else
          CRIT = C1*(C2-Y)
          if(X.gt.CRIT) then
            call EGAUSH  (X,Y,0,VAL)
          else
            call EFRACT  (X,Y,VAL)
          end if
        end if
      end if
      VOIGT = VAL/ROOTPI
C     !END
C
      return
      end
