      subroutine UPSA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Mar 11
C---- Allocates integer scratch storage for PALLE.
C     (This is version 2 of UPSA.)
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
      call HI ('UPSA')
C     !BEG
      call IGET (IS , CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+KM
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('UPSA')
C
      return
      end
