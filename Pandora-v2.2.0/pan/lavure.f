      subroutine LAVURE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 28
C---- Allocates integer scratch storage for VULTURE.
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LAVURE')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      MUX    = IN( 3)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('LAVURE')
C
      return
      end
