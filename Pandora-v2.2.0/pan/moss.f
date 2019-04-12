      subroutine MOSS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jul 11
C---- Allocates integer scratch storage for VICUNA.
C     (This is version 5 of MOSS.)
C     !DASH
      save
C     !DASH
      integer IHSSM, IN, IS, MUX
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
      equivalence (KZQ(127),IHSSM)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOSS')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+IHSSM
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MOSS')
C
      return
      end
