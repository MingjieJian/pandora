      subroutine DOVEY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1988 Apr 12
C---- Drives Line opacities initialization.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JSTIN, NPROG
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
      equivalence (KZQ(  7),JSTIN)
C     !DASH
      external LOGIN, CADMOS, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /32/
C
      call HI ('DOVEY')
C     !BEG
      if(JSTIN.eq.0) then
        call LOGIN  (NPROG)
        call CADMOS (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('DOVEY')
C
      return
      end
