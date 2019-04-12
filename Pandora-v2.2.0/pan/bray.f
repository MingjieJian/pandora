      subroutine BRAY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Sep 24
C---- Calculates PREF and Z.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, KTKIN, NPROG
      character QNAME*8
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
      equivalence (QZQ(  1),QNAME)
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
      equivalence (LEST(11),KTKIN)
C     !DASH
      external LOGIN, FONT, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /27/
C
      call HI ('BRAY')
C     !BEG
      if((KTKIN.gt.0).and.(QNAME.eq.'HYDROGEN')) then
        call LOGIN  (NPROG)
        call FONT   (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('BRAY')
C
      return
      end
