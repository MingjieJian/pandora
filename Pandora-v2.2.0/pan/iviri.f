      subroutine IVIRI
     $(IN,IS,MUX,CALLER,NW)
C
C     Rudolf Loeser, 1981 Mar 17
C---- Allocates scratch storage for APEX.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NW
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IVIRI')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NW
      IN( 3) = IN( 2)+NW
      IN( 4) = IN( 3)+NW
      IN( 5) = IN( 4)+NW
      IN( 6) = IN( 5)+NW
      IN( 7) = IN( 6)+NW
      IN( 8) = IN( 7)+NW
      MUX    = IN( 8)+NW
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IVIRI')
C
      return
      end
