      subroutine BAFFLE
     $(NO)
C
C     Rudolf Loeser, 2003 May 06
C---- Prints explanations for calculated line intensity profiles.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external RHEA, SVERRI, SNORRI, HI, BYE
C
      call HI ('BAFFLE')
C     !BEG
      if(NO.gt.0) then
C----   For (F,KS,KI)
        call RHEA   (NO, 0)
C----   For intensity sums control
        call SVERRI (NO)
C----   For dumps
        call SNORRI (NO, 0)
      end if
C     !END
      call BYE ('BAFFLE')
C
      return
      end
