      subroutine BLUB
     $(K)
C
C     Rudolf Loeser, 1982 Mar 30
C---- Prints an error message, for DURIAN.
C     (This is version 2 of BLUB.)
C     !DASH
      save
C     !DASH
      integer K, KODE, LUEO
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
      call HI ('BLUB')
C     !BEG
      call MESHED ('BLUB', 3)
      write (LUEO,100) K,KODE
  100 format(' ','Lyman Source Function Calculation: omit frequency',
     $           ' # ',I2,': could not compute WN matrix.')
      call MASHED ('BLUB')
C     !END
      call BYE ('BLUB')
C
      return
      end
