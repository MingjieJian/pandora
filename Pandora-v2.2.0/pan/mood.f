      subroutine MOOD
     $(IN,IS,MUX,CALLER)
C     Rudolf Loeser, 2000 Jan 04
C---- Allocates scratch storage for NUMB.
C     (This is version 2 of MOOD.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MOOD')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      MUX    = IN( 3)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MOOD')
C
      return
      end
