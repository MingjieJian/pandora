      subroutine IPIL
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 18 FEB 70
C---- Allocates scratch storage for SAPELE.
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
      call HI ('IPIL')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N**2
      MUX    = IN( 2)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IPIL')
C
      return
      end
