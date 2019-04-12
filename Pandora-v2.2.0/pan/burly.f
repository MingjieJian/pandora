      subroutine BURLY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 Jan 29
C---- Allocates integer scratch storage for CARROT.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, MXTAP, N, NP
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('BURLY')
C     !BEG
      call IGET (IS,  CALLER)
C
      NP = N+MXTAP
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NP
      MUX    = IN( 2)+NP
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('BURLY')
C
      return
      end
