      subroutine RILL
     $(NO,YDAMP,F,ITS)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Prints a trailer, for SHOOT.
C     !DASH
      save
C     !DASH
      real*8 F, YDAMP
      integer ITS, NO
      character ITMSS*9
C     !DASH
      external TILLER, TULLAH, PLANE, LINER, HI, BYE
C
      call HI ('RILL')
C     !BEG
      if(NO.gt.0) then
        call LINER  (1, NO)
        call TILLER (NO, F, ITS, ITMSS)
        call LINER  (1, NO)
        call TULLAH (NO, ITS, ITMSS)
        call PLANE  (NO, YDAMP)
      end if
C     !END
      call BYE ('RILL')
C
      return
      end
