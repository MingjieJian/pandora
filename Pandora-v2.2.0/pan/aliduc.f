      subroutine ALIDUC
     $(SHL)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Prints details, for CALAMUS.
C     (This is version 3 of ALIDUC.)
C     !DASH
      save
C     !DASH
      real*8 SHL
      integer LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ALIDUC')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) SHL
  100 format(' ',75X,'sum of OPACN*SHLN =',1PE16.8)
C     !END
      call BYE ('ALIDUC')
C
      return
      end
