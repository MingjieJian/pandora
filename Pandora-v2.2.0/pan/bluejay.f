      subroutine BLUEJAY
     $(NO,LINES,LINIT,IJECT)
C
C     Rudolf Loeser, 1980 Nov 14
C---- Prints a heading, and initializes printout control, for LEDGER.
C     !DASH
      save
C     !DASH
      integer IJECT, LINES, LINIT, NO
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('BLUEJAY')
C     !BEG
      call PRIAM (NO,'SPECTRUM',8)
      call LINER (1,NO)
      write (NO,100)
  100 format(' ','Summary of Electron Temperature (TE, K), ',
     $           'Brightness Temperature (TB, K), ',
     $           'and Intensity (ergs /sec /cm**2 /sterad /Hz), and'/
     $       ' ','depths of formation, for line cores ',
     $           'and the continuous spectrum, Mu=1.'//
     $       ' ','(The symbols appearing to the left of the TB values ',
     $           'identify the TB values plotted in the graph ',
     $           'that follows.)')
      call LINER (2,NO)
      LINES = 51
      LINIT = 22
      IJECT = 0
C     !END
      call BYE ('BLUEJAY')
C
      return
      end
