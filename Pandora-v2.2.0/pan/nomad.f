      subroutine NOMAD
     $(RNU,XNU,XNUC,LEVEL,WAVE)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Converts RNU (ratio of frequency units) to
C              wavelength (Angstroms).
C     (See also DAMON, MONAD, and MADON.)
C     (This is version 2 of NOMAD.)
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
      external MONAD, DIVIDE, ANGIE, HI, BYE
C
C               XNU(NSL), XNUC(NSL)
      dimension XNU(*),   XNUC(*)
C
      call HI ('NOMAD')
C     !BEG
      if(LEVEL.le.NSL) then
C----   Normal case
        call MONAD  (RNU, XNU, XNUC, LEVEL, FNU)
        call ANGIE  (FNU, WAVE)
      else
C----   Special case: K-shell ---
C       RNU is NOT ratio of frequency units, but ratio of WAVELENGTHs!
        call DIVIDE (RWKSI, RNU, WAVE)
      end if
C     !END
      call BYE ('NOMAD')
C
      return
      end
