      subroutine WUMP
     $(N,R,WN,XM)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Sets up matrix XM (= script M), for WHALE.
C     !DASH
      save
C     !DASH
      real*8 R, WN, XM
      integer J, N
C     !DASH
      external ARRMUL, NEGATE, UNITADD, HI, BYE
C
C               R(N), WN(N,N), XM(N,N)
      dimension R(*), WN(N,*), XM(N,*)
C
      call HI ('WUMP')
C     !BEG
      do 100 J = 1,N
        call ARRMUL (R,WN(1,J),XM(1,J),N)
  100 continue
      call NEGATE   (XM,(N**2))
      call UNITADD  (1,N,XM,N)
C     !END
      call BYE ('WUMP')
C
      return
      end
