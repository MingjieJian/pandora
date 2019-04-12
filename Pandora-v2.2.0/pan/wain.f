      subroutine WAIN
C
C     Rudolf Loeser, 2003 Jun 19
C---- Prints a trailer for MULIAK.
C     (This is version 4 of WAIN.)
C     !DASH
      save
C     !DASH
      integer MO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external LINER, DASHER, ABJECT, HI, BYE
C
      call HI ('WAIN')
C     !BEG
      call LINER  (1, MO)
      call DASHER (MO)
      call ABJECT (MO)
C     !END
      call BYE ('WAIN')
C
      return
      end
