      subroutine CARAT
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jul 15
C---- Allocates integer scratch storage for DIAMOND.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('CARAT')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+NT
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('CARAT')
C
      return
      end
