      subroutine GODWIT
     $(W,IW,KABS,KEMIT,KODE,KACTO,KRESN,KISLV,N,NOPAC,XLM,XLP,CORE,
     $ FMULT,BMULT,CB,CO,OPAC,CAPR,CAP,SIGM,SCAT,T1,TR,B,S1,SR,BHS,
     $ BHSD,BHSN,BHS1,BHSR,SIGSTR,BHSNMS,BCKSM,ALB,CLO,TE,V,VXS,XNE,
     $ HND,H2N,BDHM,CON,CHN,OHN,XLMXX,XLMDR,TDUST,XLMDST,EPDST,DFDST,
     $ ALBDST,XLDT,ADT,ALBDT,WAVEK,ALBK,ARRK,WAVCO,ARRCO,WAVCA,ARRCA,
     $ BANDL,BANDU,CQT,CQA,XNC,XPBL,ISWA,ISWE,H,ORES,OREM,SRES,SREM,
     $ SLO,ALO,XFRQ,XLYB,KOPAC,EMU,VEX,H1,CABS,CEMI)
C
C     Rudolf Loeser, 1986 Jul 12
C---- Controls continuum absorption and emission calculations.
C     !DASH
      save
C     !DASH
      real*8 ADT, ALB, ALBDST, ALBDT, ALBK, ALO, ARRCA, ARRCO, ARRK, B,
     $       BANDL, BANDU, BCKSM, BDHM, BHS, BHS1, BHSD, BHSN, BHSNMS,
     $       BHSR, BMULT, CABS, CAP, CAPR, CB, CEMI, CHN, CLO, CO, CON,
     $       CORE, CQA, CQT, DFDST, EMU, EPDST, FMULT, H, H1, H2N, HND,
     $       OHN, OPAC, OREM, ORES, S1, SCAT, SIGM, SIGSTR, SLO, SR,
     $       SREM, SRES, T1, TDUST, TE, TR, V, VEX, VXS, W, WAVCA,
     $       WAVCO, WAVEK, XFRQ, XLDT, XLM, XLMDR, XLMDST, XLMXX, XLP,
     $       XLYB, XNC, XNE, XPBL
      integer ISWA, ISWE, IW, KABS, KACTO, KEMIT, KISLV, KODE, KOPAC,
     $        KRESN, KUPT, N, NOPAC
      logical DUMP
C     !DASH
C     !EJECT
      external BUSTARD, SHIMMER, PLUNK, GLEAM, GLIMMER, MATACO, NETTLE,
     $         BITTER, ZEROI, GLOAM, LIATRIS, THISTLE, HI, BYE
C
      dimension W(*), IW(*)
C
C               ISWA(Nopac), ISWE(Nopac), CB(Nopac,N), BHSR(N), H2N(N),
      dimension ISWA(*),     ISWE(*),     CB(*),       BHSR(*), H2N(*),
C
C               OPAC(N), CAPR(N), CAP(N), SIGM(N), CQA(NCQ), SIGSTR(N),
     $          OPAC(*), CAPR(*), CAP(*), SIGM(*), CQA(*),   SIGSTR(*),
C
C               SCAT(N), BHSD(N), BHSN(N), B(N), BHSNMS(N), EPDST(LDU),
     $          SCAT(*), BHSD(*), BHSN(*), B(*), BHSNMS(*), EPDST(*),
C
C               BCKSM(NCSBA), KOPAC(Nopac), ARRCA(KWA,N), ARRCO(NCP,N),
     $          BCKSM(*),     KOPAC(*),     ARRCA(*),     ARRCO(*),
C
C               WAVCO(NCP), BANDL(NAB), BANDU(NAB), WAVEK(KNW), ALB(N),
     $          WAVCO(*),   BANDL(*),   BANDU(*),   WAVEK(*),   ALB(*),
C
C               SRES(N), SREM(N), CON(N), SLO(N), WAVCA(KWA), TDUST(N),
     $          SRES(*), SREM(*), CON(*), SLO(*), WAVCA(*),   TDUST(*),
C
C               TE(N), XNE(N), HND(N), BDHM(N), XLMXX(LLY), XLMDR(LLY),
     $          TE(*), XNE(*), HND(*), BDHM(*), XLMXX(*),   XLMDR(*),
C
C               XFRQ(NDT), ADT(NDT), XPBL(Lenpbl), CO(Nopac,N), VXS(N),
     $          XFRQ(*),   ADT(*),   XPBL(*),      CO(*),       VXS(*),
C
C               CABS(N,Nlin), DFDST(LDU), XLYB(Lenlyb), CLO(N), XNC(N),
     $          CABS(*),      DFDST(*),   XLYB(*),      CLO(*), XNC(*),
C
C               CEMI(N,Nlin), V(N), VEX(N), H1(N), BHS1(N), ALBDT(NDT),
     $          CEMI(*),      V(*), VEX(*), H1(*), BHS1(*), ALBDT(*),
C
C               S1(N), SR(N), ARRK(KNW,N), XLMDST(NDT), H(LIMD), TR(N),
     $          S1(*), SR(*), ARRK(*),     XLMDST(*),   H(*),    TR(*),
C
C               ORES(N), OREM(N), ALBDST(LDU), CQT(NCQ), ALO(N), T1(N),
     $          ORES(*), OREM(*), ALBDST(*),   CQT(*),   ALO(*), T1(*),
C
C               BHS(N), ALBK(N), XLDT(NDT), CHN(N), OHN(N)
     $          BHS(*), ALBK(*), XLDT(*),   CHN(*), OHN(*)
C     !EJECT
C
      call HI ('GODWIT')
C     !BEG
      call LIATRIS   (KACTO)
      call BITTER    (XLM, DUMP)
C---- Initialize absorption calculations indicators
      call ZEROI     (ISWA, 1, NOPAC)
C---- Compute miscellaneous absorption
      call SHIMMER   (XLM, XLP, N, NOPAC, CO, ISWA, XNE, H2N, TE, HND,
     $                XLMDST, DFDST, ALBDST, WAVEK, ALBK, ALB, ARRK,
     $                WAVCO, ARRCO, WAVCA, ARRCA, XLDT, ADT, ALBDT,
     $                XFRQ, SLO, CLO, ALO, CHN, OHN, BCKSM, KOPAC, CQT,
     $                CQA, BMULT)
      if(KEMIT.gt.0) then
C----   Compute Planck function
        call PLUNK   (N, XLM, TE, B)
C----   Initialize emission calculations indicators
        call ZEROI   (ISWE, 1, NOPAC)
C----   Compute miscellaneous emission
        call GLIMMER (XLM, XLP, N, NOPAC, CB, ISWE, TDUST, XLMDST,
     $                EPDST, CO, ISWA, BCKSM, KOPAC)
      end if
C---- Compute absorption and emission involving non-LTE ions
      call GLOAM     (W, IW, XLM, XLP, CORE, NOPAC, KEMIT, CO, ISWA,
     $                CB, ISWE, XPBL, N, TE, XNC, V, VXS, B, XNE,
     $                BDHM, XLMXX, XLMDR, ORES, OREM, SRES, SREM, H,
     $                KRESN, KISLV, KODE, T1, TR, S1, SR, KUPT, SLO,
     $                ALO, BCKSM, CON, XLYB, KOPAC, EMU, VEX, H1,
     $                CABS, CEMI)
      if(KEMIT.gt.0) then
C----   Compute emission components = B * absorption
        call GLEAM   (N, NOPAC, ISWE, CO, B, CB, ISWA, BCKSM, KOPAC)
      end if
C---- Debug printout (if needed)
      call NETTLE    (DUMP, XLM, N, NOPAC, KOPAC, CO, ISWA, T1, TR,
     $                CB, ISWE, S1, SR)
C---- Shuffle and combine various absorption contributions
      call BUSTARD   (XLM, KUPT, KOPAC, NOPAC, N, CO, T1, TR, FMULT,
     $                CAPR, CAP, SIGM, OPAC, SCAT, SIGSTR)
      if(KEMIT.gt.0) then
C----   Shuffle and combine various emission contributions
        call MATACO  (XLM, KUPT, KOPAC, NOPAC, N, CO, CB, BHS, BHSD,
     $                BHSN, S1, SR, T1, TR, BHS1, BHSR, CAPR, BHSNMS,
     $                FMULT)
      end if
C---- Debug printout (if needed)
      call THISTLE   (DUMP, XLM, N, KUPT, FMULT, CAPR, CAP, SIGM,
     $                OPAC, SCAT, SIGSTR, BHSN, BHSD, BHS, BHSNMS)
C     !END
      call BYE ('GODWIT')
C
      return
      end
