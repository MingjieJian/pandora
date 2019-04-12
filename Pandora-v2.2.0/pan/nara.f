      subroutine NARA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Allocates integer scratch storage for IMPALA.
C     (This is version 3 of NARA.)
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
      call HI ('NARA')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('NARA')
C
      return
      end
