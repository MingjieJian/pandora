      subroutine MADON
     $(FNU,XNU,XNUC,LEVEL,RNU)
C
C     Rudolf Loeser, 2002 Jan 17
C---- Converts frequnits to ratio of frequnits.
C     (See also MONAD, NOMAD, and DAMON.)
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
      call HI ('MADON')
C     !BEG
      if(LEVEL.le.NSL) then
C----   Normal case
        call DIVIDE (FNU, (XNUC(LEVEL)-XNU(LEVEL)), RNU)
C
      else
C----   Special case: K-shell ---
C       RNU is NOT ratio of frequency units, but of WAVELENGTHs!
C       So, must recover wavelength, and then convert wavelength
C       to ratio of wavelengths.
        call ANGIE  (FNU, WAVE)
        call DIVIDE (RWKSI, WAVE, RNU)
      end if
C     !END
      call BYE ('MADON')
C
      return
      end
