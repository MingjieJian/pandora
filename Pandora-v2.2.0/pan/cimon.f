      subroutine CIMON
     $(N,K,LU,DL,GMA,EP,BS,S,XJNU,FAB,FJN,FJJ,SLF,XRD,YRD,VXI,XQSF,
     $ SNU,SLR,FO,IMG)
C
C     Rudolf Loeser, 2005 Apr 04
C---- Computes PRD terms for CRETE.
C     (This is version 5 of CIMON.)
C     !DASH
      save
C     !DASH
      real*8 BS, DL, EP, FAB, FJJ, FJN, FO, GMA, S, SLF, SLR, SNU, VXI,
     $       XJNU, XQSF, XRD, YRD
      integer IMG, K, LU, N
C     !DASH
      external PANTHER, SLATE, GUMBO, TESLA, GALLA, MOUR, HI, BYE
C
C               DL(K), GMA(N), EP(N), BS(N), SLF(N,K), FAB(N,K), FO(N),
      dimension DL(*), GMA(*), EP(*), BS(*), SLF(*),   FAB(*),   FO(*),
C
C               FJN(N,K), FJJ(N,K), XJNU(N,K), XQSF(N,K), S(N), IMG(N),
     $          FJN(*),   FJJ(*),   XJNU(*),   XQSF(*),   S(*), IMG(*),
C
C               XRD(N,K), YRD(N,K), SNU(N,K), SLR(N,K), VXI(N,K)
     $          XRD(*),   YRD(*),   SNU(*),   SLR(*),   VXI(*)
C
      call HI ('CIMON')
C     !BEG
C---- VXI
      call PANTHER (N, K, GMA, EP, FAB, VXI)
C---- QSF
      call SLATE   (N, K, GMA, FJJ, FJN, FAB, SLF, EP, BS, XQSF, IMG,
     $              FO)
C---- SNU
      call GUMBO   (N, K, XQSF, VXI, XJNU, SNU)
C---- SLR
      call TESLA   (N, K, XRD, YRD, S, SLR, IMG, FO)
C
C---- Print
      call GALLA   (LU, N, K, DL, XJNU, VXI, XQSF, XRD, YRD, S, SNU,
     $              SLR)
C
C---- Checksums
      call MOUR    (VXI, XQSF, SNU, SLR, N, K)
C     !END
      call BYE ('CIMON')
C
      return
      end
