      subroutine VERONA
     $(CSHL,NSHL,CDSK,MRR,N,XLM,CF,CKLSHL,CKLDSK,ZAXA,ZAYA,CBHS,SIGMAS,
     $ BHSNMS,CNXP,CSFCRIT,INDSHL,INDDSK,XJNU,CSF,ITS,LAG,OPACSHL,
     $ OPACDSK,XASHL,XADSK,YASHL,YADSK,WNSHL,WNDSK,XM,RR,XJNUO,SUM1,
     $ SUM2,W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 05
C---- Computes Mean Intensity, XJNU, Continuum Source Function, CSF,
C     and codes LAG and ITS, for angle-dependent Continuum Calculations
C     in spherical atmospheres.
C     Keeps track separately of Shell components (suffix "SHL")
C     and Disk components (suffix "DSK").
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, CBHS, CDSK, CF, CKLDSK, CKLSHL, CNXP, CSF, CSFCRIT,
     $       CSHL, OPACDSK, OPACSHL, RR, SIGMAS, SUM1, SUM2, W, WNDSK,
     $       WNSHL, XADSK, XASHL, XJNU, XJNUO, XLM, XM, YADSK, YASHL,
     $       ZAXA, ZAYA
      integer INDDSK, INDSHL, ITS, IW, KODE, KODEXA, LAG, MRR, MXKNT, N,
     $        NSHL
      logical DUMP
C     !DASH
      external HIPPO, MOESIA, PASTA, ATHENA, GEREINT, MOVE1, DELPHI,
     $         GELA, HI, BYE
C
      dimension W(*), IW(*)
C
C               CDSK(N,MRR), XADSK(N,N,MRR), CKLSHL(N,N,NSHL), SUM1(N),
      dimension CDSK(*),     XADSK(*),       CKLSHL(*),        SUM1(*),
C
C               CKLDSK(N,N,MRR), ZAXA(N), ZAYA(N), XJNUO(N), SIGMAS(N),
     $          CKLDSK(*),       ZAXA(*), ZAYA(*), XJNUO(*), SIGMAS(*),
C
C               BHSNMS(N), CNXP(N), CSF(N), OPACSHL(N,N,NSHL), CBHS(N),
     $          BHSNMS(*), CNXP(*), CSF(*), OPACSHL(*),        CBHS(*),
C
C               WNDSK(N,N,MRR), YADSK(N,N,MRR), WNSHL(N,N,NSHL), RR(N),
     $          WNDSK(*),       YADSK(*),       WNSHL(*),        RR(*),
C
C               OPACDSK(N,N,MRR), XASHL(N,N,NSHL), YASHL(N,N,NSHL),
     $          OPACDSK(*),       XASHL(*),        YASHL(*),
C
C               CSHL(N,NSHL), XJNU(N), SUM2(N), XM(N,N)
     $          CSHL(*),      XJNU(*), SUM2(*), XM(*)
C
      data MXKNT /25/
C     !EJECT
C
      call HI ('VERONA')
C     !BEG
C---- Compute intermediates XA (KODEXA) and YA
      call HIPPO     (N, NSHL, CKLSHL, MRR, CKLDSK, ZAXA, ZAYA, SIGMAS,
     $                BHSNMS, CF, XASHL, XADSK, YASHL, YADSK, OPACSHL,
     $                OPACDSK, KODEXA, INDSHL, INDDSK, DUMP)
C---- Compute intermediate RR
      call MOESIA    (N, NSHL, CSHL, YASHL, WNSHL, MRR, CDSK, YADSK,
     $                WNDSK, CNXP, RR, DUMP)
C---- Compute XJNU iteratively, if possible
      if(KODEXA.eq.1) then
        call PASTA   (N, NSHL, CSHL, XASHL, WNSHL, MRR, CDSK, XADSK,
     $                WNDSK, RR, XJNU, ITS, SUM1, SUM2, XJNUO, CSFCRIT,
     $                MXKNT, DUMP)
      else
        ITS = -1
      end if
C---- Compute XJNU by matrix method, if necessary
      KODE = 1
      if((ITS.lt.0).or.(ITS.gt.MXKNT)) then
        call ATHENA  (N, XLM, NSHL, CSHL, XASHL, WNSHL, MRR, CDSK,
     $                XADSK, WNDSK, RR, XJNU, XM, KODE, W, IW, DUMP)
      end if
C---- Set XJNU equal to default values, if necessary
      if(KODE.ne.1) then
        call GEREINT (N, RR, NSHL, CSHL, XASHL, MRR, CDSK, XADSK, SUM1,
     $                XJNU, DUMP)
      end if
C---- Compute CSF
      call GELA      (XJNU, N, LAG)
      if(LAG.eq.0) then
        call DELPHI  (N, NSHL, CSHL, XASHL, YASHL, MRR, CDSK, XADSK,
     $                YADSK, XJNU, SUM1, SUM2, CSF, DUMP)
      else
        call MOVE1   (CBHS, N, CSF)
      end if
C     !END
      call BYE ('VERONA')
C
      return
      end
