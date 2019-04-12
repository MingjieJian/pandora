      subroutine ALPACA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1991 Dec 12
C---- Allocates scratch storage for VICUNA.
C     !DASH
      save
C     !DASH
      integer IHSKM, IHSSM, IN, IS, MUX
      character CALLER*(*)
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
      equivalence (KZQ(126),IHSKM)
      equivalence (KZQ(127),IHSSM)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ALPACA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+9*IHSKM
      IN( 3) = IN( 2)+4*IHSSM
      MUX    = IN( 3)+IHSSM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ALPACA')
C
      return
      end
