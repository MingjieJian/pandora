      subroutine CELLA
     $(NO)
C
C     Rudolf Loeser, 1993 Jun 04
C---- Prints a message, for DABBLE.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('CELLA')
C     !BEG
      if(NO.gt.0) then
        call LINER (2,NO)
        write (NO,100)
  100   format(' ','Listing of atmosphere model data tables has been ',
     $             'suppressed (option ATMOPRNT).')
      end if
C     !END
      call BYE ('CELLA')
C
      return
      end
