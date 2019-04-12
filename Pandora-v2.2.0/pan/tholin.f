      subroutine THOLIN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Jun 08
C---- Allocates integer scratch storage for ARRAS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, MWV, NW
      character CALLER*(*)
C     !DASH
      external IGET, ILCK, MAROZIA, HI, BYE
C
      dimension IN(*)
C
      call HI ('THOLIN')
C     !BEG
      call IGET   (IS ,CALLER)
C
      call MAROZIA (MWV)
      NW = MWV+1
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NW
      MUX    = IN( 2)+NW
C
      call ILCK   (MUX,CALLER)
C     !END
      call BYE ('THOLIN')
C
      return
      end
