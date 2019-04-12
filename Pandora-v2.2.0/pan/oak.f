      subroutine OAK
C
C     Rudolf Loeser, 1991 Nov 29
C---- Computes MPROM,
C     the Line Profile calculation mode switch,
C     for the current transition.
C
C     MPROM = 0 means: Voigt profile only;
C     MPROM = 1 means: convolve with H Stark profile.
C
C     (This is version 4 of OAK.)
C     !DASH
      save
C     !DASH
      real*8 FF, ONE, ZERO
      integer IHSSW, IOVER, MPROM
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(123),IHSSW)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(63),MPROM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external GROUSE, HI, BYE
C
      dimension FF(5)
C
      call HI ('OAK')
C     !BEG
      MPROM = 0
      if((QELSM.eq.'H  ').and.(IHSSW.eq.1)) then
        if(IOVER.eq.0) then
          MPROM = 1
        else
          call GROUSE (0, ONE, ONE, ONE, ONE, ONE, FF)
          if(FF(3).ne.ZERO) then
            MPROM = 1
          end if
        end if
      end if
C     !END
      call BYE ('OAK')
C
      return
      end
