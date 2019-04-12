      subroutine PEDGE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2006 Jan 13
C---- Allocates integer scratch storage for OPHIR.
C     (This is version 2 of PEDGE.)
C
C     (Note: NL**2 should be enough; but give it more in case
C     that might be needed in the future.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('PEDGE')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+(NSL**2)
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('PEDGE')
C
      return
      end
