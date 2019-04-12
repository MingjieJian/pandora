      subroutine WOBBLE
     $(N,BDJ,KODE)
C
C     Rudolf Loeser, 1978 Sep 21
C---- Returns with KODE=1 if any of the values of BDJ are negative,
C             with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 BDJ
      integer KODE, KOUNT, N
C     !DASH
      external  MINUSD, HI, BYE
      intrinsic min
C
C               BDJ(N)
      dimension BDJ(*)
C
      call HI ('WOBBLE')
C     !BEG
      call MINUSD (BDJ,1,N,KOUNT)
      KODE = min(KOUNT,1)
C     !END
      call BYE ('WOBBLE')
C
      return
      end
