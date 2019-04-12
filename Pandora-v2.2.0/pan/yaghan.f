      subroutine YAGHAN
     $(JPR,PRNT)
C
C     Rudolf Loeser, 2005 May 20
C---- Prints a PRD-iteration marker.
C     (This is version 3 of YAGHAN.)
C     !DASH
      save
C     !DASH
      integer JPR, NO
      logical PRNT
C     !COM  or  !DASH
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('YAGHAN')
C     !BEG
      if(PRNT) then
        call LINER (1, NO)
        write (NO,100) JPR
  100   format(' ',7('*******'),'     PRD-iteration # ',I2,'     ',
     $             9('******'))
        call LINER (1, NO)
      end if
C     !END
      call BYE ('YAGHAN')
C
      return
      end
