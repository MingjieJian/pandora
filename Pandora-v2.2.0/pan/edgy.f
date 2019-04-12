      subroutine EDGY
     $(NO,IJECT)
C
C     Rudolf Loeser, 1989 May 26.
C---- Printing utility: for form feeds.
C     (See also "DEJECT".)
C     (This is version 3 of EDGY.)
C----
C     Normal use:  input value of IJECT=1, which causes two line-feeds.
C     Special use: input value of IJECT=0, which causes a form-feed,
C                  and then IJECT is set =1 (the "normal" value).
C     !DASH
      save
C     !DASH
      integer IJECT, NO
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('EDGY')
C     !BEG
      if(NO.gt.0) then
        if(IJECT.eq.0) then
          call ABJECT (NO)
          IJECT = 1
        else
          call LINER  (2, NO)
        end if
      end if
C     !END
      call BYE ('EDGY')
C
      return
      end
