      subroutine JOEPYE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Jul 21
C---- Allocates scratch storage for PANKU.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NN
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JOEPYE')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN = N**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+NN
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+N
      IN(13) = IN(12)+NN
      MUX    = IN(13)+NN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('JOEPYE')
C
      return
      end
