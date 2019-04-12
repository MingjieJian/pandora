      subroutine NARADEL
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Mar 25
C---- Allocates integer scratch storage for LIGHT.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NVP, NVX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NARADEL')
C     !BEG
      call IGET (IS,  CALLER)
C
      NVP = NVX+3
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+NVP
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('NARADEL')
C
      return
      end
