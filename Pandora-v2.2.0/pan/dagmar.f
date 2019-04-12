      subroutine DAGMAR
     $(IU,IL)
C
C     Rudolf Loeser, 1992 Jun 10
C---- Prints an error message for PLOVER.
C     (This is version 2 of DAGMAR.)
C     !DASH
      save
C     !DASH
      integer IL, IU, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
      call HI ('DAGMAR')
C     !BEG
      call MESHED ('DAGMAR', 3)
      write (LUEO,100) IU,IL
  100 format(' ','F-editing occurred during the calculation of ',
     $           'SLF(',I2,'/',I2,').')
      call MASHED ('DAGMAR')
C     !END
      call BYE ('DAGMAR')
C
      return
      end
