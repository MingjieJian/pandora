      subroutine NALLA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1988 Jun 21
C---- Allocates scratch storage for WALLA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NP
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NALLA')
C     !BEG
      call WGET (IS ,CALLER)
C
      NP = N+2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NP
      IN( 3) = IN( 2)+NP
      MUX    = IN( 3)+NP
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NALLA')
C
      return
      end
