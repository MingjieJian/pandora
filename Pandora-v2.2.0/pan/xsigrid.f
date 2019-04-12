      subroutine XSIGRID
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1990 Dec 11
C---- Allocates scratch storage for XROTOR: determinant calculation.
C
C     old version of SIGRID, using scaling; saved 2006 Sep 06
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
      call HI ('XSIGRID')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N**2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('XSIGRID')
C
      return
      end
