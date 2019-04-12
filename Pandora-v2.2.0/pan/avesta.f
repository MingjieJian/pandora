      subroutine AVESTA
     $(W,N,MRR,K,WVL,DL,SI,SII,DI,DII,XHZ,EW,FDL,YDL)
C
C     Rudolf Loeser, 1983 Jul 28
C---- Computes integrated ray intensities.
C     WDL, WHZ, and WEW are working storage.
C     !DASH
      save
C     !DASH
      real*8 DI, DII, DL, EW, FDL, SI, SII, W, WVL, XHZ, YDL
      integer I, K, MRR, N
C     !DASH
      external HISSAR, MOVED, HI, BYE
C
      dimension W(*)
C
C               FDL(K), SII(N,K), DI(MRR,K), DII(MRR,K), DL(K), XHZ(K),
      dimension FDL(*), SII(N,*), DI(MRR,*), DII(MRR,*), DL(*), XHZ(*),
C
C               SI(N,K), EW(K), YDL(K)
     $          SI(N,*), EW(*), YDL(*)
C
      call HI ('AVESTA')
C     !BEG
      do 100 I = 1,N
        call MOVED    (SI(I,1),N,K,FDL,1,K)
        call HISSAR   (W,K,WVL,DL,FDL,XHZ,EW,YDL)
        call MOVED    (YDL,1,K,SII(I,1),N,K)
  100 continue
C
      if(MRR.gt.0) then
        do 101 I = 1,MRR
          call MOVED  (DI(I,1),MRR,K,FDL,1,K)
          call HISSAR (W,K,WVL,DL,FDL,XHZ,EW,YDL)
          call MOVED  (YDL,1,K,DII(I,1),MRR,K)
  101   continue
      end if
C     !END
      call BYE ('AVESTA')
C
      return
      end
