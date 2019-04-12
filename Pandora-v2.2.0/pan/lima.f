      subroutine LIMA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Allocates scratch storage for IMPALA.
C     (This is version 3 of LIMA.)
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
      call HI ('LIMA')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+5*N
      IN( 3) = IN( 2)+2*N
      MUX    = IN( 3)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LIMA')
C
      return
      end
