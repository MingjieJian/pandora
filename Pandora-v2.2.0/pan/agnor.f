      subroutine AGNOR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Nov 07
C---- Allocates integer scratch storage for KARDA.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NAB
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(45),NAB)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('AGNOR')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NAB
      IN( 3) = IN( 2)+NAB
      MUX    = IN( 3)+NAB
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('AGNOR')
C
      return
      end
