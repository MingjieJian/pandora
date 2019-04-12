      subroutine NIAS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Jul 14
C---- Allocates integer scratch storage for MIAS.
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
      call HI ('NIAS')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+NL
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('NIAS')
C
      return
      end
