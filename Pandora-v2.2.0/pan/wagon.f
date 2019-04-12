      subroutine WAGON
     $(N,TE,Z,ZT,DTE,VEC,W,IW)
C
C     Rudolf Loeser, 2002 Apr 18
C---- Sets up temperature gradients.
C     (This is version 2 of WAGON.)
C     !DASH
      save
C     !DASH
      real*8 DTE, TE, VEC, W, Z, ZT
      integer IW, N
      logical ISO
C     !DASH
      external KONSTD, ZERO1, SIGMA, MOVE1, PACIFIC, HI, BYE
C
      dimension W(*), IW(*)
C
C               TE(N), Z(N), ZT(N), DTE(N), VEC(N)
      dimension TE(*), Z(*), ZT(*), DTE(*), VEC(*)
C
      call HI ('WAGON')
C     !BEG
      call KONSTD    (TE,1,N,TE(1),ISO)
      if(ISO) then
        call ZERO1   (DTE,N)
        call ZERO1   (ZT,N)
      else
        call MOVE1   (TE,N,VEC)
        call PACIFIC (Z,VEC,DTE,N,W,IW)
C
        call MOVE1   (TE,N,VEC)
        call SIGMA   (N,Z,VEC,ZT,'ZT',W,IW)
      end if
C     !END
      call BYE ('WAGON')
C
      return
      end
