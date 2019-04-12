      subroutine PILLAGE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Allocates integer scratch storage for TURBOT.
C     !DASH
      save
C     !DASH
      integer IN, IS, ITKZA, ITN1R, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KZQ(157),ITN1R)
      equivalence (KZQ(202),ITKZA)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('PILLAGE')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N*(ITN1R*ITKZA+1)
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('PILLAGE')
C
      return
      end
