      subroutine KACHIN
     $(NO,XMU,CMU,WMU,LFLX,LG)
C
C     Rudolf Loeser, 1983 Mar 07
C---- Prints angle integration weights.
C     (This is version 2 of KACHIN.)
C     !DASH
      save
C     !DASH
      real*8 CMU, WMU, XMU
      integer I, LFLX, LG, NO
C     !DASH
      external PRIAM, LINER, HI, BYE
C
C               XMU(LG), CMU(LG), WMU(LG)
      dimension XMU(*),  CMU(*),  WMU(*)
C
      call HI ('KACHIN')
C     !BEG
      if(NO.gt.0) then
        call PRIAM (NO, 'DIRECTIONS', 10)
C
        call LINER (3, NO)
        write (NO,100)
  100   format(' ','Weights for explicit angle integrations.')
        call LINER (2, NO)
C
        if(LFLX.gt.0) then
          write (NO,101) (I,XMU(I),CMU(I),WMU(I),I=1,LG)
  101     format(' ',6X,'Direction',14X,'WN',14X,'WH'/
     $           ' ',8X,'Cosines',9X,'Weights',9X,'Weights'//
     $           ' ',12X,'XMU',13X,'CMU',13X,'WMU'//
     $         5(' ',I3,1PE12.4,2E16.8/))
        else
          write (NO,102) (I,XMU(I),CMU(I),I=1,LG)
  102     format(' ',6X,'Direction'/
     $           ' ',8X,'Cosines',9X,'Weights'//
     $           ' ',12X,'XMU',13X,'CMU'//
     $         5(' ',I3,1PE12.4,E16.8/))
        end if
      end if
C     !END
      call BYE ('KACHIN')
C
      return
      end
