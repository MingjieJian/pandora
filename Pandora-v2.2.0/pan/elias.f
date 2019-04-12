      subroutine ELIAS
     $(IN,IS,MUX,CALLER,MTR)
C
C     Rudolf Loeser, 1991 Mar 22
C---- Allocates scratch storage for KYNAN.
C     !DASH
      save
C     !DASH
      integer IN, IS, MTR, MUX
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ELIAS')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+MTR
      IN( 3) = IN( 2)+MTR
      MUX    = IN( 3)+MTR
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('ELIAS')
C
      return
      end
