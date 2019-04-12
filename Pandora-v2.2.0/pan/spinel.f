      subroutine SPINEL
     $(E)
C
C     Rudolf Loeser, 1984 Jul 06
C---- Prints a heading, for FINCH.
C     (This is version 2 of SPINEL.)
C     !DASH
      save
C     !DASH
      real*8 E
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
      call HI ('SPINEL')
C     !BEG
      write (LUEO,100) E
  100 format(' ','Calculation of the Injection Function FINJ(V,E), ',
     $           'for Hydrogen ionization.'//
     $       ' ','E =',1PE16.8)
      call LINER (2, LUEO)
C     !END
      call BYE ('SPINEL')
C
      return
      end
