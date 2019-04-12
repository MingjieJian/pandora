      subroutine ULTAMI
     $(NO,STRING)
C
C     Rudolf Loeser, 1998 Dec 21
C---- Prints the finale.
C     !DASH
      save
C     !DASH
      integer NO
      character STRING*(*)
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('ULTAMI')
C     !BEG
      if(NO.gt.0) then
        call LINER  (3,NO)
        write (NO,100) STRING
  100   format(' ','End of ',A)
        call DASHER (NO)
      end if
C     !END
      call BYE ('ULTAMI')
C
      return
      end
