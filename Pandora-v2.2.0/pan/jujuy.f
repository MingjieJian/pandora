      subroutine JUJUY
     $(IN,IS,MUX,CALLER,NL)
C
C     Rudolf Loeser, 2004 Dec 14
C---- Allocates integer scratch storage for KALT.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL
      character CALLER*(*)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JUJUY')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NL
      MUX    = IN( 2)+NL
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('JUJUY')
C
      return
      end
