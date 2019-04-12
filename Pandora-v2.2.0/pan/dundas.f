      subroutine DUNDAS
     $(E)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Prints a heading, for MINCH.
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
      call HI ('DUNDAS')
C     !BEG
      write (LUEO,100) E
  100 format(' ','Calculation of the Injection Function FJIN(N,E), ',
     $           'for Hydrogen excitation.'//
     $       ' ','E =',1PE16.8)
      call LINER  (2, LUEO)
C     !END
      call BYE ('DUNDAS')
C
      return
      end
