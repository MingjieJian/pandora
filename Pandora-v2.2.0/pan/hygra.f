      subroutine HYGRA
     $(N,TE,Z,KZXST,ZT,DTE,VEC,W,IW)
C
C     Rudolf Loeser, 1989 Sep 12
C---- Sets up initial temperature gradients.
C     !DASH
      save
C     !DASH
      real*8 DTE, TE, VEC, W, Z, ZT
      integer IW, KZXST, N
      logical ISO
C     !DASH
      external WAGON, KONSTD, ZERO1, HI, BYE
C
      dimension W(*), IW(*)
C
C               TE(N), Z(N), ZT(N), DTE(N), VEC(N)
      dimension TE(*), Z(*), ZT(*), DTE(*), VEC(*)
C
      call HI ('HYGRA')
C     !BEG
      call KONSTD  (Z, 1, N, Z(1), ISO)
      if(ISO.or.(KZXST.eq.0)) then
        call ZERO1 (DTE, N)
        call ZERO1 (ZT , N)
      else
        call WAGON (N, TE, Z, ZT, DTE, VEC, W, IW)
      end if
C     !END
      call BYE ('HYGRA')
C
      return
      end
