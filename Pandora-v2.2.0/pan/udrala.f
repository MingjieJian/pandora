      subroutine UDRALA
     $(NO)
C
C     Rudolf Loeser, 2003 May 06
C---- Prints explanations for calculated continuum intensities.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external RHEA, SVERRI, SNORRI, HI, BYE
C
      call HI ('UDRALA')
C     !BEG
      if(NO.gt.0) then
C----   For (F,KS,KI)
        call RHEA   (NO, 1)
C----   For intensity sums control
        call SVERRI (NO)
C----   For dumps
        call SNORRI (NO, 1)
      end if
C     !END
      call BYE ('UDRALA')
C
      return
      end
