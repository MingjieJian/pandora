      subroutine LOTOR
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 2000 Jun 16
C---- Allocates scratch storage for ISMENE.
C     (This is version 2 of LOTOR.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LOTOR')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LOTOR')
C
      return
      end
