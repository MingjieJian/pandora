      subroutine INNER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 20 Apr 70
C---- Allocates scratch storage for ATOM.
C     !DASH
      save
C     !DASH
      integer IN, IS, KWK, MUX, NL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('INNER')
C     !BEG
      call WGET (IS,  CALLER)
C
      KWK = (NL*(NL-1))/2
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KWK
      IN( 3) = IN( 2)+KWK
      IN( 4) = IN( 3)+KWK
      IN( 5) = IN( 4)+KWK
      IN( 6) = IN( 5)+KWK
      IN( 7) = IN( 6)+KWK
      IN( 8) = IN( 7)+KWK
      IN( 9) = IN( 8)+KWK
      IN(10) = IN( 9)+KWK
      IN(11) = IN(10)+KWK
C
      IN(12) = IN(11)+KWK
      IN(13) = IN(12)+KWK
      IN(14) = IN(13)+KWK
      IN(15) = IN(14)+KWK
      IN(16) = IN(15)+KWK
      IN(17) = IN(16)+KWK
      IN(18) = IN(17)+KWK
      IN(19) = IN(18)+KWK
      IN(20) = IN(19)+KWK
      IN(21) = IN(20)+KWK
C
      IN(22) = IN(21)+KWK
      IN(23) = IN(22)+KWK
      IN(24) = IN(23)+KWK
      IN(25) = IN(24)+KWK
      IN(26) = IN(25)+KWK
      IN(27) = IN(26)+KWK
      IN(28) = IN(27)+KWK
      IN(29) = IN(28)+KWK
      MUX    = IN(29)+KWK
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('INNER')
C
      return
      end
