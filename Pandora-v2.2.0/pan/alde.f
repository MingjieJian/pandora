      subroutine ALDE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1979 Jul 12
C---- Drives Rates calculation.
C     (This is version 3 of ALDE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NOION, NPROG
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
      equivalence (KZQ( 94),NOION)
C     !DASH
      external LOGIN, SALLY, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /8/
C
      call HI ('ALDE')
C     !BEG
      if(NOION.le.0) then
        call LOGIN  (NPROG)
        call SALLY  (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('ALDE')
C
      return
      end
