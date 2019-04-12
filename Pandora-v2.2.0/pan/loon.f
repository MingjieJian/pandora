      subroutine LOON
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1983 Mar 16
C---- Allocates scratch storage for FAKIR.
C     (This is version 2 of LOON.)
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
      call HI ('LOON')
C     !BEG
      call WGET (IS ,CALLER)
C
      N2 = N**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+N2
      IN( 4) = IN( 3)+N2
      MUX    = IN( 4)+N2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LOON')
C
      return
      end
