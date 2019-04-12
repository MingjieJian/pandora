      subroutine KOENIG
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Jul 25
C---- Allocates integer scratch storage for GLIDE.
C     !DASH
      save
C     !DASH
      integer IN, IS, KKX, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(12),KKX)
C     !DASH
      external  IGET, ILCK, HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('KOENIG')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+KKX
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('KOENIG')
C
      return
      end
