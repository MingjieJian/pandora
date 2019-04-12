      subroutine PIDDOCK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Oct 05
C---- Allocates scratch storage for TURBOT.
C     !DASH
      save
C     !DASH
      integer IN, IS, ITN1R, MUX, N, NIT, NL, NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
C     !DASH
      external  WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('PIDDOCK')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNL = N*NL
      NIT = N*(ITN1R+1)
C     !EJECT
      IN( 1) = IS
      IN( 2) = IN( 1)+NNL
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NNL
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+N
      IN(20) = IN(19)+N
C
      IN(21) = IN(20)+NNL
      IN(22) = IN(21)+N
      IN(23) = IN(22)+NIT
      IN(24) = IN(23)+NL
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+NIT
      IN(28) = IN(27)+NNL
      IN(29) = IN(28)+NNL
      IN(30) = IN(29)+NNL
C
      IN(31) = IN(30)+N
      IN(32) = IN(31)+N
      IN(33) = IN(32)+N
      IN(34) = IN(33)+N
      IN(35) = IN(34)+N
      IN(36) = IN(35)+NNL
      IN(37) = IN(36)+N
      IN(38) = IN(37)+NNL
      IN(39) = IN(38)+N
      IN(40) = IN(39)+N
C
      IN(41) = IN(40)+N
      IN(42) = IN(41)+N
      MUX    = IN(42)+NNL
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('PIDDOCK')
C
      return
      end
