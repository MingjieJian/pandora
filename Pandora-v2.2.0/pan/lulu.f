      subroutine LULU
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Oct 22
C---- Allocates scratch storage for KURNAI.
C     (This is version 4 of LULU.)
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
      call HI ('LULU')
C     !BEG
      call WGET (IS,  CALLER)
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
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      MUX    = IN(12)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LULU')
C
      return
      end
