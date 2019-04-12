      subroutine MERU
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1981 May 07
C---- Allocates scratch storage for CRONOS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N2
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MERU')
C     !BEG
      call WGET (IS ,CALLER)
C
      N2 = N**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+N2
      MUX    = IN( 3)+N2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MERU')
C
      return
      end
