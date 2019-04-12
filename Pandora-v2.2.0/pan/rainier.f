      subroutine RAINIER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 11
C---- Allocates integer scratch storage for KALI.
C     !DASH
      save
C     !DASH
      integer IBC, IN, IS, KOMNP, KOMNT, KOMNV, MUX, N, NAB
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(45),NAB)
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
      equivalence (KZQ( 69),KOMNP)
      equivalence (KZQ( 70),KOMNT)
      equivalence (KZQ( 68),KOMNV)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('RAINIER')
C     !BEG
      call IGET (IS ,CALLER)
C
      IBC = ((KOMNP*KOMNT*KOMNV)+2)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+IBC
      MUX    = IN( 5)+NAB
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('RAINIER')
C
      return
      end
