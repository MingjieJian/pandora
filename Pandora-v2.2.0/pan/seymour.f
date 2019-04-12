      subroutine SEYMOUR
     $(XLM,KAVER)
C
C     Rudolf Loeser, 1993 Sep 09
C---- Sets KAVER=1 if Averaged Line Opacity is defined at this
C     wavelength, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 ALOMA, ALOMI, XLM
      integer KAVER, KWA
      logical ANY, INRANGE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(57),KWA)
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
      equivalence (RZQ(142),ALOMI)
      equivalence (RZQ(143),ALOMA)
C     !DASH
      external HI, BYE
C
      call HI ('SEYMOUR')
C     !BEG
      KAVER = 0
C
      ANY = KWA.gt.0
      if(ANY) then
        INRANGE = (XLM.ge.ALOMI).and.(XLM.le.ALOMA)
        if(INRANGE) then
          KAVER = 1
        end if
      end if
C     !END
      call BYE ('SEYMOUR')
C
      return
      end
