      subroutine RADISH
     $(FIN,REF)
C
C     Rudolf Loeser, 2005 Apr 18
C---- Checks options compatibility, for RATRACE.
C     (This is version 2 of RADISH.)
C     !DASH
      save
C     !DASH
      integer LUEO
      logical FIN, REF
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
      call HI ('RADISH')
C     !BEG
      if(REF.and.(.not.FIN)) then
        call MESHED ('RADISH', 1)
        write (LUEO,100) REF,FIN
  100   format(' ','Error in RADISH: computing weight matrix by ',
     $             'ray-trace (RT) method.'//
     $         ' ','REFlect = ',L5,' while FINite = ',L5,
     $             'does not make sense.')
        call ABORT
      end if
C     !END
      call BYE ('RADISH')
C
      return
      end
