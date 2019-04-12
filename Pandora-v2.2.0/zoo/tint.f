      subroutine TINT
     $(F,FM,FP,FL,F0,GOOD)
C
C     Rudolf Loeser, 2007 Jan 08
C---- Tests (? and adjusts) F, for CUBINT.
C     See (97 Jan 31).
C     !DASH
      save
C     !DASH
      real*8 D0, DL, F, F0, FL, FM, FP, ONE
      logical GOOD
C     !DASH
      intrinsic sign, abs
C
      data ONE /1.D0/
C
C     !BEG
      if(sign(ONE,(FM-FL)).eq.sign(ONE,(FP-FL))) then
C----   FM and FP do not bracket FL (they "are on the same side of FL")
        GOOD = sign(ONE,(FL-F)).ne.sign(ONE,(F0-F))
        if(.not.GOOD) then
C----     FL and F0 do not bracket F
          DL = abs(FL-F)
          D0 = abs(F0-F)
          if(DL.lt.D0) then
            F = FL
          else
            F = F0
          end if
        end if
      else
C----   FM and FP bracket FL (they "are on opposite sides of FL")
        F = FL
      end if
C     !END
C
      return
      end
