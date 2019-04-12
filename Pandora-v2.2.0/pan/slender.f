      subroutine SLENDER
     $(NO,TIT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Prints heading for an iterative summary.
C     (This is version 2 of SLENDER.)
C     !DASH
      save
C     !DASH
      integer NO
      character SS*10, TB*10, TIT*(*)
C     !DASH
      external ABJECT, HI, BYE
C
      data TB, SS /'$$$$$$$$$$', '$$      $$'/
C
      call HI ('SLENDER')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        write (NO,100) TB,SS,SS,TIT,SS,SS,TB
  100   format(' ',A10/
     $         ' ',A10/
     $         ' ',A10/
     $         ' ',13X,'Iterative behavior of  ',A,'  relative to ',
     $             'its final value.',T85,'(For graph instead, turn ',
     $             'option SUMGRAF on.)'/
     $         ' ',A10/
     $         ' ',A10/
     $         ' ',A10)
      end if
C     !END
      call BYE ('SLENDER')
C
      return
      end
