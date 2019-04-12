      subroutine LAPSANA
     $(XLM,KSTAT)
C
C     Rudolf Loeser, 1988 Dec 12
C---- Sets KSTAT=1 if Statistical Line Opacity is defined at this
C     wavelength, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 CURMA, CURMI, XLM
      integer KNW, KSTAT
      logical ANY, INRANGE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(25),KNW)
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
      equivalence (RZQ( 48),CURMI)
      equivalence (RZQ( 49),CURMA)
C     !DASH
      external HI, BYE
C
      call HI ('LAPSANA')
C     !BEG
      KSTAT = 0
C
      ANY = KNW.gt.0
      if(ANY) then
        INRANGE = (XLM.ge.CURMI).and.(XLM.le.CURMA)
        if(INRANGE) then
          KSTAT = 1
        end if
      end if
C     !END
      call BYE ('LAPSANA')
C
      return
      end
