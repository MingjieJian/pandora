      subroutine RACHEL
     $(IU,IL,INDEX)
C
C     Rudolf Loeser, 1984 Feb 16
C---- Finds the THULE table index for transition (IU/IL).
C     (This is version 2 of RACHEL.)
C     !DASH
      save
C     !DASH
      integer IL, INDEX, IU, LUEO
      logical GOOD
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SOPHIA, SLIM, MESHED, LINER, ABORT, HI, BYE
C
      call HI ('RACHEL')
C     !BEG
      call SLIM     (IU, IL, INDEX, GOOD)
C
      if(.not.GOOD) then
        call MESHED ('RACHEL', 1)
        write(LUEO,100) IU,IL
  100   format(' ','Transition (IU/IL) = (',I2,'/',I2,')'//
     $         ' ','A record entry for this transition is presumed ',
     $             'to exist, but does not occur in the index.')
        call LINER  (2, LUEO)
        call SOPHIA (LUEO)
        call ABORT
      end if
C     !END
      call BYE ('RACHEL')
C
      return
      end
