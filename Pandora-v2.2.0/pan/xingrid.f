      subroutine XINGRID
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1990 Dec 11
C---- Allocates scratch storage for MOTOR, i.e. matrix inversion.
C     (This is version 5 of INGRID.)
C
C     Old version of INGRID, using scaling; saved 2006 Sep 06
C
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
      call HI ('XINGRID')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N*N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('XINGRID')
C
      return
      end
