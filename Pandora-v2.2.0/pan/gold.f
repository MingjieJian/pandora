      subroutine GOLD
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2006 Feb 27
C---- Allocates scratch storage for CROWN.
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
      call HI ('GOLD')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('GOLD')
C
      return
      end
