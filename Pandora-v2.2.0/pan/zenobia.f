      subroutine ZENOBIA
     $(NO,K,LF,EMUF,DL,WVL,WVNUM,WTAB,WINT,TF,SF,FHZ,NLTE,KLIN,TCF,KTF,
     $ KILROY,ISB1,LDL,DDL,IJECT,W)
C
C     Rudolf Loeser, 1981 Sep 01
C---- Computes and prints a Flux profile (plus auxiliary quantities).
C     (This is version 2 of ZENOBIA.)
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, EMUF, FHZ, SF, TCF, TF, W, WINT, WTAB, WVL, WVNUM,
     $       dummy
      integer IBT, IEW, IFAN, IFIN, IFIS, IJECT, IN, IRES, IS, ISB1,
     $        IXHZ, IXLA, K, KFUNC, KLIN, KRES, KSTAR, KTF, LDL, LF,
     $        MOX, NLTE, NO, jummy
      logical KILROY
C     !DASH
      external LECCE, SAPHIRE, RIVAL, FABLE, NABIZ, WGIVE, HI, BYE
C
      dimension W(*)
C
C               EMUF(LF), FHZ(KM), WINT(KM), DL(KM), SF(KM), WVNUM(KM),
      dimension EMUF(*),  FHZ(*),  WINT(*),  DL(*),  SF(*),  WVNUM(*),
C
C               TCF(KM), TF(KM), WTAB(KM), DDL(LDLMX)
     $          TCF(*),  TF(*),  WTAB(*),  DDL(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IEW   ),(IN( 2),IBT   ),(IN( 3),IFIN  ),(IN( 4),IFAN  ),
     $(IN( 5),IFIS  ),(IN( 6),IXLA  ),(IN( 7),IXHZ  ),(IN( 8),IRES  )
C
      data KFUNC,KSTAR /2, 0/
C     !EJECT
C
      call HI ('ZENOBIA')
C     !BEG
C     (Get, and allocate, W allotment)
      call NABIZ   (IN, IS, MOX, 'ZENOBIA')
C
C---- Get "Flux /HZ", FHZ
      call LECCE   (K, WINT, TF, FHZ)
C---- Compute auxiliary functions
      call SAPHIRE (W, WVL, K, DL, KFUNC, FHZ, W(IEW), W(IBT), W(IFIN),
     $              W(IFAN), W(IFIS), W(IXLA), W(IXHZ))
C---- Compute residuals
      call RIVAL   (FHZ, TCF, W(IRES), K, KRES)
C---- Print
      call FABLE   (NO, KFUNC, NLTE, KSTAR, EMUF, LF, WVL, DL, WVNUM,
     $              WTAB, K, FHZ, W(IFIN), W(IFIS), W(IFAN), W(IBT),
     $              dummy, jummy, jummy, jummy, W(IRES), TF, SF, KLIN,
     $              TCF, KTF, KILROY, LDL, DDL, ISB1, IJECT)
C
C     (Give back W allotment)
      call WGIVE   (W, 'ZENOBIA')
C     !END
      call BYE ('ZENOBIA')
C
      return
      end
