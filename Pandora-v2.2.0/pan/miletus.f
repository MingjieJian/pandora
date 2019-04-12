      subroutine MILETUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1999 Nov 05
C---- Allocates integer scratch storage for TOURMAL.
C     (This is version 4 of MILETUS.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MILETUS')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NL
      IN( 3) = IN( 2)+NL
      MUX    = IN( 3)+NL
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('MILETUS')
C
      return
      end
