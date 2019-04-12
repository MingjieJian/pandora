      subroutine GELLA
     $(IQINP,NO)
C
C     Rudolf Loeser, 1989 Mar 10
C---- Prints section header, for DABBLE.
C     (This is version 2 of GELLA.)
C     !DASH
      save
C     !DASH
      integer IQINP, NO
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('GELLA')
C     !BEG
      if(NO.gt.0) then
        if(IQINP.gt.0) then
          call PRIAM (NO,'INPUT',5)
          call LINER (2,NO)
          write (NO,100)
  100     format(' ','Listing of other input parameters for this run.',
     $               52X,'(Options INDAPRNT, AINDPRNT)')
C
        else
          call LINER (3,NO)
          write (NO,101)
  101     format(' ','Listing of other input parameters for this run ',
     $               'has been suppressed.',32X,
     $               '(Options INDAPRNT, AINDPRNT)')
        end if
        call LINER   (3,NO)
      end if
C     !END
      call BYE ('GELLA')
C
      return
      end
