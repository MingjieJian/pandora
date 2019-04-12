      subroutine KADAR
     $(WAVES,NWV)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Sets up XLCOW, the CO dump wavelength, to be equal to that value
C     of WAVES that is closest to XLCOD.
C     !DASH
      save
C     !DASH
      real*8 WAVES, XLCOD, XLCOW, ZERO
      integer I, NWV
      logical YES
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
      equivalence (RZQ( 98),XLCOD)
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
      equivalence (REST( 4),XLCOW)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NEARSD, WITHIN, HI, BYE
C
C               WAVES(NWV)
      dimension WAVES(*)
C     !EJECT
C
      call HI ('KADAR')
C     !BEG
      XLCOW = ZERO
C
      call WITHIN   (WAVES(1), XLCOD, WAVES(NWV), 0, YES)
      if(YES) then
        call NEARSD (WAVES, NWV, XLCOD, I)
        if((I.gt.1).and.(I.lt.NWV)) then
          XLCOW = WAVES(I)
        end if
      end if
C     !END
      call BYE ('KADAR')
C
      return
      end
