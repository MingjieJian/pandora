      subroutine KYLVON
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 2000 Jun 19
C---- Allocates scratch storage for KILYDD & ARVON.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KYLVON')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KYLVON')
C
      return
      end
