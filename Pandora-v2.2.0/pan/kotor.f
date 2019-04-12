      subroutine KOTOR
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Jul 14
C---- Allocates integer scratch storage for MOTOR & ROTOR.
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
      call HI ('KOTOR')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+N
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('KOTOR')
C
      return
      end
