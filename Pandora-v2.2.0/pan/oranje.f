      subroutine ORANJE
     $(NO,INC)
C
C     Rudolf Loeser, 1981 Aug 10
C---- Writes a heading, for ORINOCO.
C     (This is version 2 of ORANJE.)
C     !DASH
      save
C     !DASH
      integer NO
      logical INC
      character BLANKS*11, Q*11
C     !DASH
      external LINER, SETC, HI, BYE
C
      dimension Q(4)
C
      data BLANKS /'           '/
C
      call HI ('ORANJE')
C     !BEG
      if(NO.gt.0) then
C
        if(INC) then
          Q(1) = '  Incident '
          Q(2) = ' Radiation '
          Q(3) = '    term   '
          Q(4) = '    INCD   '
        else
          call SETC (Q, 1, 4, BLANKS)
        end if
C
        call LINER  (1, NO)
        write (NO,100) Q
  100   format(' ',90X,'Line'/
     $         ' ',5X,'Statist.',2X,'Continuum',15X,'Statist.',
     $             4X,'Partial',5X,'Final',9X,'Line',8X,'Center',
     $             5X,'Net',3X,'Integrated',A11/
     $         ' ',6X,'Equil.',4X,'Transfer',5X,'Planck',5X,'Equil.',
     $             8X,'B',10X,'B',10X,'Source',6X,'Optical',
     $             3X,'Radiat.',4X,'Mean',3X,A11/
     $         ' ',2X,2(5X,'term',2X),1X,4(3X,'Function'),6X,'Function',
     $             6X,'Depth',4X,'Bracket',2X,'Intensity',A11//
     $         ' ',2X,'I',3X,'EPSILON',4X,'DELTA',9X,'B',10X,'BS',
     $             9X,'BA',9X,'BF',12X,'S',11X,'TAU',7X,'RHO',6X,'JBAR',
     $             3X,A11)
        call LINER  (1, NO)
      end if
C     !END
      call BYE ('ORANJE')
C
      return
      end
