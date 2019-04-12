      subroutine MONAD
     $(RNU,XNU,XNUC,LEVEL,FNU)
C
C     Rudolf Loeser, 2002 Jan 17
C---- Converts ratio of frequnits to frequnits.
C     (See also MADON, NOMAD, and DAMON.)
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
      external  DIVIDE, ANGIE, HI, BYE
C
C               XNU(NSL), XNUC(NSL)
      dimension XNU(*),   XNUC(*)
C
      call HI ('MONAD')
C     !BEG
      if(LEVEL.le.NSL) then
C----   Normal case
        FNU = RNU*(XNUC(LEVEL)-XNU(LEVEL))
C
      else
C----   Special case: K-shell ---
C       RNU is NOT ratio of frequency units, but of WAVELENGTHs!
C       So, must recover wavelength, and then convert wavelength
C       to frequency units.
        call DIVIDE (RWKSI, RNU, WAVE)
        call ANGIE  (WAVE, FNU)
      end if
C     !END
      call BYE ('MONAD')
C
      return
      end
