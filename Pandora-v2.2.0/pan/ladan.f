      subroutine LADAN
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Allocates integer scratch storage for DALLAN.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LADAN')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      MUX    = IN( 2)+KM
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('LADAN')
C
      return
      end
