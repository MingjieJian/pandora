      subroutine CREMONA
     $(CMU,LG,N,XLM,CF,CKL,ZAXA,ZAYA,CBHS,SIGMAS,BHSNMS,CNXP,CSFCRIT,
     $ INDEX,XJNU,CSF,ITS,LAG,OPAC,XA,YA,CI,WN,XM,RR,XJNUO,SUM1,SUM2,
     $ W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes Mean Intensity, XJNU, Continuum Source Function, CSF,
C     and codes LAG and ITS, for angle-dependent Continuum Calculations
C     in plane-parallel atmospheres.
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, CBHS, CF, CI, CKL, CMU, CNXP, CSF, CSFCRIT, OPAC,
     $       RR, SIGMAS, SUM1, SUM2, W, WN, XA, XJNU, XJNUO, XLM, XM,
     $       YA, ZAXA, ZAYA
      integer INDEX, ITS, IW, KODE, KODEXA, LAG, LG, MXKNT, N
      logical DUMP
C     !DASH
      external AGORA, JULIUS, GELA, FELIX, GODEMAR, OWEIN, MOVE1, GILO,
     $         ALINDA, HI, BYE
C
      dimension W(*), IW(*)
C
C               CMU(LG), CKL(N,N,LG), ZAXA(N), ZAYA(N), CBHS(N), RR(N),
      dimension CMU(*),  CKL(*),      ZAXA(*), ZAYA(*), CBHS(*), RR(*),
C
C               SIGMAS(N), BHSNMS(N), CI(N,LG), XJNUO(N), OPAC(N,N,LG),
     $          SIGMAS(*), BHSNMS(*), CI(*),    XJNUO(*), OPAC(*),
C
C               SUM2(N), XM(N,N), CSF(N), XJNU(N), SUM1(N), WN(N,N,LG),
     $          SUM2(*), XM(*),   CSF(*), XJNU(*), SUM1(*), WN(*),
C
C               XA(N,N,LG), YA(N,N,LG), CNXP(N)
     $          XA(*),      YA(*),      CNXP(*)
C
      data MXKNT /25/
C     !EJECT
C
      call HI ('CREMONA')
C     !BEG
C---- Compute intermediates XA (KODEXA) and YA
      call AGORA     (N, LG, CKL, ZAXA, ZAYA, SIGMAS, BHSNMS, CF, XA,
     $                YA, OPAC, KODEXA, INDEX, DUMP)
C---- Set up angle-integration weights
      call JULIUS    (N, LG, CMU, CI)
C---- Compute intermediate RR
      call ALINDA    (N, LG, YA, WN, CNXP, CI, RR, DUMP)
C
C---- Compute XJNU iteratively, if possible
      if(KODEXA.eq.1) then
        call FELIX   (N, LG, XA, WN, RR, XJNU, ITS, CI, SUM1, SUM2,
     $                XJNUO, CSFCRIT, MXKNT, DUMP)
      else
        ITS = -1
      end if
C
C---- Compute XJNU by matrix method, if necessary
      KODE = 1
      if((ITS.lt.0).or.(ITS.gt.MXKNT)) then
        call GODEMAR (N, LG, XLM, XA, WN, RR, XJNU, XM, KODE, CI, W,
     $                IW, DUMP)
      end if
C
      if(KODE.ne.1) then
C----   Set XJNU equal to default values, if necessary
        call OWEIN   (N, LG, XA, RR, CI, SUM1, XJNU, DUMP)
      end if
C
C---- Compute CSF
      call GELA      (XJNU, N, LAG)
      if(LAG.eq.0) then
        call GILO    (N, LG, XA, YA, XJNU, CI, SUM1, SUM2, CSF, DUMP)
      else
        call MOVE1   (CBHS, N, CSF)
      end if
C     !END
      call BYE ('CREMONA')
C
      return
      end
