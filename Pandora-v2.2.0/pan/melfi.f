      subroutine MELFI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Jul 31
C---- Allocates integer scratch storage for ATOM.
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MELFI')
C     !BEG
      call IGET (IS ,CALLER)
C
      KWK = (NL*(NL-1))/2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KWK
      IN( 3) = IN( 2)+KWK
      IN( 4) = IN( 3)+KWK
      IN( 5) = IN( 4)+KWK
      IN( 6) = IN( 5)+KWK
      MUX    = IN( 6)+KWK
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MELFI')
C
      return
      end
