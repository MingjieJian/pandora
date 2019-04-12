      subroutine JAGER
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 19(* Feb 02
C---- Allocates scratch storage for MAURY.
C     (This is version 2 of JAGER.)
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
      call HI ('JAGER')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JAGER')
C
      return
      end
