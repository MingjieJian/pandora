      subroutine RATTAN
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1989 Dec 08
C---- Allocates scratch storage for RATRACE and QUADRAT.
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
      call HI ('RATTAN')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N**2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('RATTAN')
C
      return
      end
