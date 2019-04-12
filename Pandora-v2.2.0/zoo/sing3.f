      subroutine SING3
     $(R)
C     Rudolf Loeser, 1979 Apr 05
C---- Common key index code for "SING" routines
C     !DASH
      save
C     !DASH
      real*4 R, RA, RB, RC, RD
C     !COM
      common /SINGRC/ RA,RB,RC,RD
C     !DASH
      data  RA,RB,RC,RD /0.375, 0.58984375, 0.21875, 0.0390625/
C
C     !BEG
      if(R.gt.RB) then
        R = R-RC
      else
        R = R+RD
      end if
C     !END
C
      return
      end
