      subroutine SIGRID
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1990 Dec 11
C---- Allocates scratch storage for ROTOR: determinant calculation.
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
      call HI ('SIGRID')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N**2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('SIGRID')
C
      return
      end
