      subroutine OKRA
     $(N,Z,K,DL,XJNU,NOLD,ZOLD,KOLD,DLOLD,XJOLD,XJINT,INTZ,IU,IL)
C
C     Rudolf Loeser, 1977 Sep 23
C---- Inter(extra)polates Jnu, for ZAP.
C     Start with
C        XJOLD, an array of the form (NOLD,KOLD) = (ZOLD,DLOLD).
C        Take logs of XJOLD.
C        Interpolate from ZOLD to Z, giving
C        XJINT, an array of the form (N,KOLD) = (Z,DLOLD).
C        Interpolate from DLOLD to DL, giving
C        XJNU, an array of the form (N,K) = (Z,DL).
C        Take antilogs of XJNU.
C
C     (This is version 2 of OKRA.)
C     !DASH
      save
C     !DASH
      real*8 DL, DLOLD, XJINT, XJNU, XJOLD, Z, ZOLD
      integer I, IL, INTZ, IU, J, K, KOLD, N, NOLD
C     !DASH
      external DERE, MOVE1, ARLES, BAUTO, HI, BYE
C
C               XJNU(N,K), ZOLD(NOLD), XJINT(N,KOLD), XJOLD(NOLD,KOLD),
      dimension XJNU(N,*), ZOLD(*),    XJINT(N,*),    XJOLD(NOLD,*),
C
C               DLOLD(KOLD), Z(N), DL(K)
     $          DLOLD(*),    Z(*), DL(*)
C
      call HI ('OKRA')
C     !BEG
C---- Compute logs
      call ARLES    (XJOLD, NOLD, KOLD, IU, IL)
C
      if(INTZ.gt.0) then
C----   Z-interpolation
        do 100 J = 1,KOLD
          call DERE (ZOLD, 1, XJOLD(1,J), 1, NOLD,
     $               Z,    1, XJINT(1,J), 1, N,    2)
  100   continue
      else
        call MOVE1  (XJOLD, (NOLD*KOLD), XJINT)
      end if
C
C---- DL-interpolation
      do 101 I = 1,N
        call DERE   (DLOLD, 1, XJINT(I,1), N, KOLD,
     $               DL,    1, XJNU (I,1), N, K,    2)
  101 continue
C
C---- Compute antilogs
      call BAUTO    (XJNU, N, K)
C     !END
      call BYE ('OKRA')
C
      return
      end
