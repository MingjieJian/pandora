      subroutine RENARD
     $(XJNU,APD,TW,YAYB,VEC,NDT,N,RJ)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Computes RJ for SINEW.
C     !DASH
      save
C     !DASH
      real*8 APD, RJ, TW, VEC, XJNU, YAYB
      integer I, N, NDT
C     !DASH
      external MOVED, FOX, HI, BYE
C
C               XJNU(N,NDT), APD(NDT), TW(NDT), YAYB(NDT), VEC(NDT),
      dimension XJNU(N,*),   APD(*),   TW(*),   YAYB(*),   VEC(*),
C
C               RJ(N)
     $          RJ(*)
C
      call HI ('RENARD')
C     !BEG
      do 100 I = 1,N
        call MOVED (XJNU(I,1),N,NDT,YAYB,1,NDT)
        call FOX   (NDT,YAYB,APD,VEC,TW,RJ(I))
  100 continue
C     !END
      call BYE ('RENARD')
C
      return
      end
