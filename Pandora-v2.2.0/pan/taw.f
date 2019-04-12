      subroutine TAW
C
C     Rudolf Loeser, 1972 Nov 30
C---- Drives wrap-up of general output, and of general processing.
C     !DASH
      save
C     !DASH
      integer IRUNT, IXSTA, LUCS, LUPD, LURO, LUSD
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
      equivalence (KZQ( 11),IXSTA)
      equivalence (KZQ( 93),IRUNT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),LURO )
      equivalence (LUNITS(36),LUPD )
      equivalence (LUNITS(14),LUCS )
      equivalence (LUNITS(16),LUSD )
C     !DASH
C     !EJECT
      external SPY, SAY, GILES, BYEBYE, AGATHA, ABJECT, NAVAGA, DASHER,
     $         AARDVRK, PERFORM, PRINCE, FLAGON, LINER, ALEVIN, PRIAM,
     $         RITZ, HI, BYE
C
      call HI ('TAW')
C     !BEG
C---- Optional debug checksums
      call PRINCE     (LURO, LUCS)
C
C---- "Normal" wrap-up
      if((IXSTA.gt.0).and.(IRUNT.le.0)) then
        call NAVAGA   (LURO, 'SIGN-OFF', 8)
        call LINER    (1, LURO)
      else
        call PRIAM    (LURO, 'SIGN-OFF', 8)
      end if
C
      if(IXSTA.gt.0) then
        if(IRUNT.gt.0) then
C----     Detailed scratch files usage data
          call DASHER (LURO)
          call SPY    (LURO)
C----     Detailed storage blocks lengths
          call ALEVIN (LURO)
C----     Optional execution data
          call DASHER (LURO)
          call GILES  (LURO)
C----     Matrix elements range data
          call AGATHA (LURO)
C
          call DASHER (LURO)
          call ABJECT (LURO)
        end if
        call SAY      (LURO)
        call ABJECT   (LURO)
        call DASHER   (LURO)
      end if
C---- Optional file use data
      call RITZ       (LURO)
C---- "DIVIDE" message
      call FLAGON     (LURO)
C
C---- Final printout signoff
      call DASHER     (LURO)
      call AARDVRK    (LURO, IRUNT, LUSD)
C---- Final performance data archive record
      call PERFORM    (LUPD, LUSD)
C
C---- Close out Hi/Bye/Abort System
      call BYEBYE
C     !END
      call BYE ('TAW')
C
      return
      end
