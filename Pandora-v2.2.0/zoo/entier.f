      subroutine ENTIER
     $(X,RES)
C     Rudolf Loeser, 1979 Apr 18
C---- RES will be the (algebraically) largest whole number
C     not greater than X.
C     !DASH
      save
C     !DASH
      real*4 ONE, RES, X
C     !DASH
      intrinsic aint
C
      data ONE /1.E0/
C
C     !BEG
      RES = aint(X)
      if(X.lt.RES) then
        RES = RES-ONE
      end if
C     !END
C
      return
      end
