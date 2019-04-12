      subroutine WERNER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 10
C---- Allocates integer scratch storage for BRAMBLE.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL, NNL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('WERNER')
C     !BEG
      call IGET (IS ,CALLER)
C
      NNL = N*NL
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NNL
      IN( 4) = IN( 3)+NNL
      MUX    = IN( 4)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('WERNER')
C
      return
      end
