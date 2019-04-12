      subroutine MACADAM
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1976 Nov 26
C---- Allocates scratch storage for COCOS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MACADAM')
C     !BEG
      call WGET (IS ,CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNL
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NNL
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+NNL
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      MUX    = IN(10)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MACADAM')
C
      return
      end
