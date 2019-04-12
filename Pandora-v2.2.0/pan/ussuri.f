      subroutine USSURI
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1985 Jan 04
C---- Allocates scratch storage for AMUR.
C     (This is version 2 of USSURI.)
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
      call HI ('USSURI')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      MUX    = IN( 5)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('USSURI')
C
      return
      end
