      subroutine NAPOPE
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1998 Oct 15
C---- Allocates integer scratch storage for PACIFIC.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NAPOPE')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('NAPOPE')
C
      return
      end
