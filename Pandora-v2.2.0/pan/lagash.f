      subroutine LAGASH
     $(IN,IS,MUX,CALLER,M5,N3,N)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Allocates scratch storage for NIDABA.
C     !DASH
      save
C     !DASH
      integer IN, IS, M5, MUX, N, N3
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LAGASH')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N3*M5
      IN( 3) = IN( 2)+N3*N3
      MUX    = IN( 3)+N3*N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LAGASH')
C
      return
      end
