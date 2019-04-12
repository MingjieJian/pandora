      subroutine LAURI
     $(IN,IS,MUX,CALLER,K)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Allocates scratch storage for KOWRI.
C     !DASH
      save
C     !DASH
      integer IN, IS, K, MUX
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LAURI')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+K*K
      MUX    = IN( 2)+K
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LAURI')
C
      return
      end
