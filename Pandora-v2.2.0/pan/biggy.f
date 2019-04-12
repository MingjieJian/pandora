      subroutine BIGGY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Allocates integer scratch storage for BLOAT.
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
      call HI ('BIGGY')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      MUX    = IN( 3)+KM
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('BIGGY')
C
      return
      end
