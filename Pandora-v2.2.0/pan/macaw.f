      subroutine MACAW
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1972 Feb 01
C---- Allocates scratch storage for CREAM.
C     !DASH
      save
C     !DASH
      integer IN, IS, LZM, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(13),LZM)
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MACAW')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+LZM
      IN( 3) = IN( 2)+LZM
      MUX    = IN( 3)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MACAW')
C
      return
      end
