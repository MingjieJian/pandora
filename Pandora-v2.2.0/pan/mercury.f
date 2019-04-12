      subroutine MERCURY
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1971 Jan 06
C---- Allocates scratch storage for PAMPAS.
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
      call HI ('MERCURY')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      MUX    = IN( 3)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MERCURY')
C
      return
      end
