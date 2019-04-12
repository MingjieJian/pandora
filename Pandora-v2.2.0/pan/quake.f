      subroutine QUAKE
     $(NO,TITLE,PRNT)
C
C     Rudolf Loeser, 1995 Feb 13
C---- Prints Header and advice, for FUNGUS.
C     (This is version 2 of QUAKE.)
C     !DASH
      save
C     !DASH
      integer MOPRN, NO
      logical PRNT
      character TITLE*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(150),MOPRN)
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('QUAKE')
C     !BEG
      PRNT = MOPRN.gt.0
      call LINER    (2,NO)
      if(PRNT) then
        call DASHER (NO)
        call LINER  (1,NO)
        write (NO,100) TITLE
  100   format(' ','Data for the built-in model of ',A,52X,
     $             '(To omit printing, set MOPRNT = 0)')
        call LINER  (1,NO)
      else
        write (NO,101) TITLE
  101   format(' ','(To print data for the built-in model of ',A,
     $             ', set MOPRNT = 1)')
      end if
C     !END
      call BYE ('QUAKE')
C
      return
      end
