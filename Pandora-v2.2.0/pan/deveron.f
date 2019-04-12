      subroutine DEVERON
     $(NO)
C
C     Rudolf Loeser, 1991 Jun 11
C---- Prints heading for the Iteration Trend Summary.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('DEVERON')
C     !BEG
      call ABJECT (NO)
      write (NO,100)
  100 format(' ','Iteration Trend Summary',67X,
     $           '(Option SUMTREND and options ITER...)'//
     $       ' ','The iterative behavior at each depth during the ',
     $           'last three iterations is summarized by the ',
     $           'following codes:'/
     $       ' ','(minus): sign changed in the last iteration'/
     $       ' ','      O: oscillatory, small change'/
     $       ' ','      P: oscillatory, large change'/
     $       ' ','      M: monotonic, small change'/
     $       ' ','      L: monotonic, large change'/
     $       ' ','(blank): no significant change (if all codes of ',
     $           'a given line are blank, then it is not printed).')
      call LINER  (1, NO)
      write (NO,101)
  101 format(' ','The number in brackets is the index of that depth ',
     $           'where  abs[ F(next-to-last iter) / F(last iter) ',
     $           '-1 ] is greatest.')
      call LINER  (2, NO)
C     !END
      call BYE ('DEVERON')
C
      return
      end
