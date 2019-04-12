      subroutine ZAMIDAR
     $(NLTE,KSTAR,JSAV,K,DL,WVNUM,WTAB,WVL,YHZ,YY,KODE,MUX,MYX,TCX,
     $ KTX,RES,KRES,KLIN,ISB1,KILROY,NO,LDL,DDL,IJECT,W)
C
C     Rudolf Loeser, 2000 Jul 19
C---- Supervises line-profile printout, and saves data for
C     Spectrum Summary.
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, RES, TCX, W, WTAB, WVL, WVNUM, YHZ, YY, dummy
      integer IBT, IEW, IJECT, IN, IS, ISB1, IVEC, IYAN, IYIN, IYIS,
     $        JSAV, K, KFUNC, KLIN, KODE, KRES, KSTAR, KTX, LDL, MOX,
     $        MUX, MYX, NLTE, NO, jummy
      logical KILROY
C     !DASH
C     !EJECT
      external MARDI, SAPHIRE, SQUANTO, RIVAL, FABLE, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WVNUM(KM), KODE(KM), YHZ(KM), MUX(KM), MYX(KM), DL(KM),
      dimension WVNUM(*),  KODE(*),  YHZ(*),  MUX(*),  MYX(*),  DL(*),
C
C               WTAB(KM), TCX(KM), RES(KM), YY(KM), DDL(LDLMX)
     $          WTAB(*),  TCX(*),  RES(*),  YY(*),  DDL(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IEW   ),(IN( 2),IBT   ),(IN( 3),IYIN  ),(IN( 4),IYAN  ),
     $(IN( 5),IYIS  ),(IN( 6),IVEC  )
C
      data KFUNC /1/
C
      call HI ('ZAMIDAR')
C     !BEG
C     (Get, and allocate, W allotment)
      call MARDI   (IN, IS, MOX, 'ZAMIDAR')
C
C---- Compute auxiliary functions
      call SAPHIRE (W, WVL, K, DL, KFUNC, YHZ, W(IEW), W(IBT),
     $              W(IYIN), W(IYAN), W(IYIS), dummy, W(IVEC))
C---- Save line-center data for Spectrum Summary
      call SQUANTO (JSAV, WVL, DL, K, YHZ, MUX, YY, MYX, W(IBT),
     $              KODE, NLTE)
C---- Compute residuals
      call RIVAL   (YHZ, TCX, RES, K, KRES)
C---- Print
      call FABLE   (NO, KFUNC, NLTE, KSTAR, dummy, jummy, WVL, DL,
     $              WVNUM, WTAB, K, YHZ, W(IYIN), W(IYIS), W(IYAN),
     $              W(IBT), YY, MUX, MYX, KODE, RES, dummy, dummy,
     $              KLIN, TCX, KTX, KILROY, LDL, DDL, ISB1, IJECT)
C
C     (Give back W allotment)
      call WGIVE   (W, 'ZAMIDAR')
C     !END
      call BYE ('ZAMIDAR')
C
      return
      end
