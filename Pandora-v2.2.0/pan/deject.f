      subroutine DEJECT
     $(NO,IJECT)
C
C     Rudolf Loeser, 1989 May 26
C---- Printing utility: for form-feeds.
C     (See also "EDGY".)
C     (This is version 2 of DEJECT.)
C----
C     Normal use:  input value of IJECT=0, which causes a form-feed.
C     Special use: input value of IJECT=1, which causes two line-feeds,
C                  and then IJECT is set =0 (the "normal" value).
C     !DASH
      save
C     !DASH
      integer IJECT, NO
C     !DASH
      external LINER, ABJECT, HI, BYE
C
      call HI ('DEJECT')
C     !BEG
      if(NO.gt.0) then
        if(IJECT.eq.1) then
          call LINER  (2,NO)
          IJECT = 0
        else
          call ABJECT (NO)
        end if
      end if
C     !END
      call BYE ('DEJECT')
C
      return
      end
