      subroutine DAMON
     $(WAVE,XNU,XNUC,LEVEL,RNU)
C
C     Rudolf Loeser, 1980 Mar 17
C---- Converts wavelength (Angstroms) to
C              RNU (ratio of frequency units).
C     (See also NOMAD, MONAD, and MADON.)
C     !DASH
      save
C     !DASH
      real*8 FNU, RNU, RWKSI, WAVE, XNU, XNUC
      integer LEVEL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(  3),RWKSI)
C     !DASH
      external DIVIDE, ANGIE, MADON, HI, BYE
C
C               XNU(NSL), XNUC(NSL)
      dimension XNU(*),   XNUC(*)
C
      call HI ('DAMON')
C     !BEG
      if(LEVEL.le.NSL) then
C----   Normal case
        call ANGIE  (WAVE, FNU)
        call MADON  (FNU, XNU, XNUC, LEVEL, RNU)
C
      else
C----   Special case: K-shell ---
C       needs ratio of WAVELENGTHs, NOT of frequency units.
        call DIVIDE (RWKSI, WAVE, RNU)
      end if
C     !END
      call BYE ('DAMON')
C
      return
      end
