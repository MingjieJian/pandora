      subroutine INLOTH
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 15
C---- Allocates integer scratch storage for SNIT.
C     (This is version 2 of INLOTH.)
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
      call HI ('INLOTH')
C     !BEG
      call IGET   (IS ,CALLER)
C
      call MAROZIA (MWV)
      NW = MWV+1
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NW
      IN( 3) = IN( 2)+NW
      IN( 4) = IN( 3)+NW
      MUX    = IN( 4)+NW
C
      call ILCK   (MUX,CALLER)
C     !END
      call BYE ('INLOTH')
C
      return
      end
