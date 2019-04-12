      subroutine VALENS
     $(LU)
C
C     Rudolf Loeser, 1982 Mar 04
C---- Sets up the appropriate LUN for any printed output from the
C     Continuum Calculations.
C     !DASH
      save
C     !DASH
      integer IOVER, LU, MO, MOMET, NO
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(17),MOMET)
      equivalence (LEST( 2),IOVER)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external HI, BYE
C
      call HI ('VALENS')
C     !BEG
      if((MOMET.eq.0).and.(IOVER.eq.1)) then
        LU = NO
      else if(IOVER.eq.0) then
        LU = NO
      else
        LU = MO
      end if
C     !END
      call BYE ('VALENS')
C
      return
      end
