      subroutine ANGIE
     $(GIVEN,OTHER)
C
C     Rudolf Loeser, 1971 Mar 31
C---- Converts frequency units to Angstroms, or vice versa.
C     !DASH
      save
C     !DASH
      real*8 CON16, GIVEN, OTHER
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
      call HI ('ANGIE')
C     !BEG
      call RIGEL  (16, CON16)
      call DIVIDE (CON16, GIVEN, OTHER)
C     !END
      call BYE ('ANGIE')
C
      return
      end
