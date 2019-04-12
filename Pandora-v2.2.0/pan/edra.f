      subroutine EDRA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Allocates scratch storage for CADOR.
C     !DASH
      save
C     !DASH
      integer IN, IS, KOMNP, KOMNT, KOMNV, MUX, N, NCP
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(44),NCP)
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('EDRA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NCP
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+KOMNV
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+KOMNP
      IN(13) = IN(12)+KOMNT
      MUX    = IN(13)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('EDRA')
C
      return
      end
