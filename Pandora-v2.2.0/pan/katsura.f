      subroutine KATSURA
     $(W,IW,KK,N,DMP1,SN,PA,PG,OMD,DLC,FNDT,EP,BS,IMG,FO,GA,CP,
     $ XJBAR,S)
C
C     Rudolf Loeser, 2004 May 07
C---- Computes S by the DIRECT solution, either
C     for the entire set of depths (KK = 1), or for the inner
C     section beginning at KK (1 .lt. KK .le. N).
C
C     (This is version 2 of KATSURA.)
C     !DASH
      save
C     !DASH
      real*8 BS, CP, DLC, EP, FNDT, FO, GA, OMD, PA, PG, S, SN, W,
     $       XJBAR
      integer IMG, ITER, ITMX, IW, KK, N
      logical DMP1
C     !DASH
      external KATTI, BRASS, BATTI, HI, BYE
C
      dimension W(*), IW(*)
C
C               PA(N,N), PG(N,N), XJBAR(N), IMG(N), SN(N), BS(N), S(N),
      dimension PA(N,*), PG(N,*), XJBAR(*), IMG(*), SN(*), BS(*), S(*),
C
C               FNDT(N), OMD(N), DLC(N), EP(N), FO(N), GA(N), CP(N)
     $          FNDT(*), OMD(*), DLC(*), EP(*), FO(*), GA(*), CP(*)
C
      data ITMX /1/
C
      call HI ('KATSURA')
C     !BEG
C---- Set up initial S
      call KATTI   (KK, N, SN, IMG, FO, S)
C
      do 100 ITER = 1,ITMX
C----   Compute initial Jbar (used only for updating S)
        call BRASS (KK, N, S, PA, PG, OMD, DLC, FNDT, GA, CP, IMG, FO,
     $              DMP1, XJBAR)
C----   Compute updated, final S
        call BATTI (KK, N, XJBAR, EP, BS, S)
  100 continue
C     !END
      call BYE ('KATSURA')
C
      return
      end
