      subroutine MULE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 Sep 16
C---- Allocates scratch storage for MEADOW.
C     (This is version 3 of MULE.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MULE')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+N*NSL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MULE')
C
      return
      end
