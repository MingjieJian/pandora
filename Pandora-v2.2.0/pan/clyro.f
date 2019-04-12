      subroutine CLYRO
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1998 Mar 10
C---- Allocates scratch storage for CYNON.
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
      call HI ('CLYRO')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N*N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('CLYRO')
C
      return
      end
