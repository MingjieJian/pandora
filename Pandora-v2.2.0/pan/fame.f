      subroutine FAME
     $(CPJ,RCP,RNU,MRP,YW,RJ,UPJ,ITAU,N,KOOL,XNUJ, RKMI,RKJ)
C
C     Rudolf Loeser, 1974 Jun 14
C---- Computes RKJ(itau) and RKM(itau).
C     (This is version 2 of FAME.)
C     !DASH
      save
C     !DASH
      real*8 CPJ, F, RCP, RJ, RK, RKJ, RKMI, RNU, UPJ, XNUJ, YW
      integer ITAU, MRP, N
      logical KOOL
C     !DASH
      external FATE, DURAS, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), UPJ(N), YW(N,MRX+1)
      dimension RNU(*),     RCP(*),     UPJ(*), YW(*)
C
      call HI ('FAME')
C     !BEG
      call FATE  (CPJ,XNUJ,RJ,RCP(MRP),RNU(MRP),UPJ(ITAU),KOOL,RKMI,F)
C
      call DURAS (MRP,RNU,RCP,YW,ITAU,N,KOOL, RK)
      RK = F*RK
C
      RKJ = RK+RKMI
C     !END
      call BYE ('FAME')
C
      return
      end
