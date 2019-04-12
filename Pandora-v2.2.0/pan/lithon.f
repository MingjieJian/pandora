      subroutine LITHON
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Jun 08
C---- Allocates scratch storage for ARRAS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, MWV, NW
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, MAROZIA, HI, BYE
C
      dimension IN(*)
C
      call HI ('LITHON')
C     !BEG
      call WGET   (IS ,CALLER)
C
      call MAROZIA (MWV)
      NW = MWV+1
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NW
      MUX    = IN( 2)+NW
C
      call WLCK   (MUX,CALLER)
C     !END
      call BYE ('LITHON')
C
      return
      end
