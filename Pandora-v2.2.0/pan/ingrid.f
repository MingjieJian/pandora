      subroutine INGRID
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1990 Dec 11
C---- Allocates scratch storage for MOTOR, i.e. matrix inversion.
C     (This is version 5 of INGRID.)
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
      call HI ('INGRID')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+N*N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('INGRID')
C
      return
      end
