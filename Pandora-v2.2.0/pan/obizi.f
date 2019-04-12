      subroutine OBIZI
     $(KFUNC,KSTAR,WVL,K,KLIN,KILROY,SPHERE,DL,WVNUM,TC,FAN,FHZ,BT,
     $ MUX,MYX,YY,KODE,TF,SF,LDL,DDL)
C
C     Rudolf Loeser, 1991 Aug 07
C---- Writes profile data in Special Spectrum Save file (including
C     optional FONTENLA format).
C     !DASH
      save
C     !DASH
      real*8 BT, DDL, DL, FAN, FHZ, SF, TC, TF, WVL, WVNUM, YY
      integer I, K, KFUNC, KLIN, KODE, KSTAR, LDL, MUX, MYX
      logical KILROY, SPHERE
C     !DASH
      external DARIUS, DECAN, RAUDIS, HI, BYE
C
C               DL(KM), WVNUM(KM), FAN(KM), FHZ(KM), MUX(KM), KODE(KM),
      dimension DL(*),  WVNUM(*),  FAN(*),  FHZ(*),  MUX(*),  KODE(*),
C
C               TF(KM), SF(KM), MYX(KM), TC(KM), YY(KM), DDL(LDLMX),
     $          TF(*),  SF(*),  MYX(*),  TC(*),  YY(*),  DDL(*),
C
C               BT(KM)
     $          BT(*)
C
      call HI ('OBIZI')
C     !BEG
      call DARIUS   (KFUNC, WVL, LDL, DDL, KSTAR, K, KLIN, KILROY)
C
      do 100 I = 1,K
        call DECAN  (KFUNC, I, DL(I), WVNUM(I), TC(I), FHZ(I), FAN(I),
     $               MUX(I), MYX(I), YY(I), KODE(I), TF(I), SF(I),
     $               SPHERE)
  100 continue
C
      if(KFUNC.eq.1) then
        call RAUDIS (WVL, K, DL, FAN, BT, MUX, MYX, YY)
      end if
C     !END
      call BYE ('OBIZI')
C
      return
      end
