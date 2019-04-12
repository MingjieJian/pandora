      subroutine RUEBLI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Allocates scratch storage for CARROT.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, MUX, MXTAP, N
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
      equivalence (KZQ(201),MXTAP)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('RUEBLI')
C     !BEG
      M = N+MXTAP
C
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+M
      IN( 3) = IN( 2)+M
      IN( 4) = IN( 3)+M
      IN( 5) = IN( 4)+M
      IN( 6) = IN( 5)+M
      IN( 7) = IN( 6)+M
      IN( 8) = IN( 7)+M
      IN( 9) = IN( 8)+M
      IN(10) = IN( 9)+M
      MUX    = IN(10)+M
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('RUEBLI')
C
      return
      end
