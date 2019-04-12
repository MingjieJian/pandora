      subroutine CAPRA
     $(KWSS)
C
C     Rudolf Loeser, 2002 Nov 27
C---- Sets weighting printout control.
C     (This is version 2 of CAPRA.)
C     !DASH
      save
C     !DASH
      integer IWEIT, KWSS
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
      equivalence (KZQ(186),IWEIT)
C     !DASH
      external KERMESS, HI, BYE
C
      call HI ('CAPRA')
C     !BEG
      if(IWEIT.eq.1) then
        call KERMESS ('MO', KWSS)
      else if(IWEIT.eq.2) then
        call KERMESS ('NO', KWSS)
      else
        KWSS = 0
      end if
C     !END
      call BYE ('CAPRA')
C
      return
      end
