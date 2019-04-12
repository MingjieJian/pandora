      subroutine KAFUE
     $(NO,LAB)
C
C     Rudolf Loeser, 2005 Nov 30
C---- Prints a legend. for the ATOM printout.
C     (This is version 2 of KAFUE.)
C     !DASH
      save
C     !DASH
      integer NO
      character LAB*(*)
C     !DASH
      external HI, BYE
C
      call HI ('KAFUE')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) LAB
  100   format(' ',20X,'>> More options and information for ',A,
     $             ' are given in Section 19 of "About PANDORA".')
        if((LAB.eq.'CE').or.(LAB.eq.'CI')) then
          write (NO,101)
  101     format(' ',23X,'The relevant control switch settings were ',
     $               'printed in  INPUT NOTES  , above.')
        end if
      end if
C     !END
      call BYE ('KAFUE')
C
      return
      end
