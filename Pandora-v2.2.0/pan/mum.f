      subroutine MUM
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 17
C---- Allocates scratch storage for LILY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, MW, N, NB, NIL, NL, NP, NT, NX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ( 14),NIL  )
C     !DASH
      external  WGET, WLCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C     !EJECT
C
      call HI ('MUM')
C     !BEG
      call WGET (IS,  CALLER)
C
      NX = N*NT
      NB = max(NX,(N*NL))
      MW = max((2*NIL+1),1)
      NP = 3*N
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NT
      IN( 3) = IN( 2)+NX
      IN( 4) = IN( 3)+NX
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+NB
      IN( 7) = IN( 6)+NB
      IN( 8) = IN( 7)+NX
      IN( 9) = IN( 8)+NT
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+NX
C
      IN(12) = IN(11)+NB
      IN(13) = IN(12)+NX
      IN(14) = IN(13)+NX
      IN(15) = IN(14)+NX
      IN(16) = IN(15)+NP
      IN(17) = IN(16)+N
      IN(18) = IN(17)+NX
      IN(19) = IN(18)+NX
      IN(20) = IN(19)+N
      IN(21) = IN(20)+MW
C
      IN(22) = IN(21)+NX
      IN(23) = IN(22)+NX
      IN(24) = IN(23)+N
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+N
      IN(28) = IN(27)+N*NL
      IN(29) = IN(28)+NB
      IN(30) = IN(29)+NP
      IN(31) = IN(30)+NB
C
      MUX    = IN(31)+NX
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MUM')
C
      return
      end
