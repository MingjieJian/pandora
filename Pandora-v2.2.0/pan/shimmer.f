      subroutine SHIMMER
     $(XLM,XLP,N,NOPAC,CO,ISWA,XNE,H2N,TE,HND,XLMDST,DFDST,ALBDST,
     $ WAVEK,ALBK,ALB,ARRK,WAVCO,ARRCO,WAVCA,ARRCA,XLDT,ADT,ALBDT,
     $ XFRQ,SLO,CLO,ALO,CHN,OHN,BCKSM,KOPAC,CQT,CQA,BMULT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes components of opacity not depending on populations data.
C     (This is version 4 of SHIMMER.)
C     !DASH
      save
C     !DASH
      real*8 ADT, ALB, ALBDST, ALBDT, ALBK, ALO, ARRCA, ARRCO, ARRK,
     $       BCKSM, BMULT, CHN, CLO, CO, CQA, CQT, DFDST, H2N, HND, OHN,
     $       SLO, TE, WAVCA, WAVCO, WAVEK, XFRQ, XLDT, XLM, XLMDST, XLP,
     $       XNE
      integer ISWA, KOPAC, N, NOPAC
      logical DOIT, NEWAVE, NEWCHN, NEWDAT, NEWH2N, NEWHND, NEWOHN,
     $        NEWTE, NEWXNE
C     !COM
C---- SENNA       as of 2007 Jan 12
      parameter   (LCSBA=54)
      real*8      CSBA,CSBO
      integer     LCSBA,NCSBA
      character   LABSBA*16
      dimension   CSBA(LCSBA),CSBO(LCSBA),LABSBA(LCSBA)
      common      /SENNA1/ NCSBA
      common      /SENNA2/ CSBA
      common      /SENNA3/ LABSBA
      common      /SENNA4/ CSBO
C     Checksums of quantities used to compute the various components of
C     the background absorption and the background emisssion.
C     These checksums         M  U  S  T        be updated whenever the
C              corresponding quantities are recomputed.
C
C        1 TE                 2 V                  3 XNE
C        4 HND                5 BDHM               6 TDUST
C        7 H2N                8 CON
C        9 H(nlev)           10 H(bd)             11 H(nk)
C       12 He-I(nlev)        13 He-I(bd)          14 He-I(nk)
C       15 HE-II(nlev)       16 He-II(bd)         17 He-II(nk)
C       18 C-I(nlev)         19 C-I(bd)           20 C-I(nk)
C       21 Si-I(nlev)        22 Si-I(bd)          23 Si-I(nk)
C       24 Al-I(nlev)        25 Al-I(bd)          26 Al-I(nk)
C       27 Mg-I(nlev)        28 Mg-I(bd)          29 Mg-I(nk)
C       30 Fe-I(nlev)        31 Fe-I(bd)          32 Fe-I(nk)
C       33 Na-I(nlev)        34 Na-I(bd)          35 Na-I(nk)
C       36 Ca-I(nlev)        37 Ca-I(bd)          38 Ca-I(nk)
C       39 VM
C       40 O-I(nlev)         41 O-I(bd)           42 O-I(nk)
C       43 H1
C       44 S-I(nlev)         45 S-I(bd)           46 S-I(nk)
C       47 CHN               48 OHN
C       49 O-II(nlev)        50 O-II(bd)          51 O-II(nk)
C       52 O-III(nlev)       53 O-III(bd)         54 O-III(nk)
C     .
C     !DASH
C     !EJECT
      external ELSCAT, EGGAN, BIDARKA, KAZANKA, KOLYMA, CRABBY, FLABBY,
     $         WARTS, HI, BYE
C
C               XNE(N), TE(N), H2N(N), HND(N), ISWA(Nopac), WAVCO(NCP),
      dimension XNE(*), TE(*), H2N(*), HND(*), ISWA(*),     WAVCO(*),
C
C               XLMDST(NDT), ALBDST(LDU), BCKSM(NCSBA), ALO(N), SLO(N),
     $          XLMDST(*),   ALBDST(*),   BCKSM(*),     ALO(*), SLO(*),
C
C               ARRCO(NCP,N), DFDST(LDU), ADT(NDT), ALBDT(NDT), ALB(N),
     $          ARRCO(*),     DFDST(*),   ADT(*),   ALBDT(*),   ALB(*),
C
C               WAVCA(KWA), ARRCA(KWA,N), XLDT(NDT), XFRQ(NDT), CLO(N),
     $          WAVCA(*),   ARRCA(*),     XLDT(*),   XFRQ(*),   CLO(*),
C
C               CQT(NCQ), CQA(NCQ), KOPAC(Nopac), CO(Nopac,N), ALBK(N),
     $          CQT(*),   CQA(*),   KOPAC(*),     CO(*),       ALBK(*),
C
C               WAVEK(KNW), ARRK(KNW,N), CHN(N), OHN(N)
     $          WAVEK(*),   ARRK(*),     CHN(*), OHN(*)
C
      call HI ('SHIMMER')
C     !BEG
      NEWAVE = XLM.ne.XLP
      NEWTE  = BCKSM( 1).ne.CSBA( 1)
      NEWXNE = BCKSM( 3).ne.CSBA( 3)
      NEWHND = BCKSM( 4).ne.CSBA( 4)
      NEWH2N = BCKSM( 7).ne.CSBA( 7)
      NEWCHN = BCKSM(47).ne.CSBA(47)
      NEWOHN = BCKSM(48).ne.CSBA(48)
C     !EJECT
C---- Electron scattering
      NEWDAT = NEWXNE
      call WARTS     ( 3, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call ELSCAT  ( 3, XNE, N, NOPAC, CO)
        ISWA( 3) = 1
      end if
C
C---- X-ray absorption
      NEWDAT = NEWHND.or.NEWAVE
      call WARTS     (26, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call EGGAN   (26, XLM, N, NOPAC, HND, CO)
        ISWA(26) = 1
      end if
C
C---- H2 Rayleigh scattering
      NEWDAT = NEWH2N.or.NEWAVE
      call WARTS     (28, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call BIDARKA (28, XLM, N, NOPAC, H2N, CO)
        ISWA(28) = 1
      end if
C
C---- OH bound-free absorption
      NEWDAT = NEWOHN.or.NEWTE.or.NEWAVE
      call WARTS     (40, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call FLABBY  (40, XLM, N, NOPAC, TE, OHN, CO)
        ISWA(40) = 1
      end if
C
C---- CH bound-free absorption
      NEWDAT = NEWCHN.or.NEWTE.or.NEWAVE
      call WARTS     (41, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call CRABBY  (41, XLM, N, NOPAC, TE, CHN, CO)
        ISWA(41) = 1
      end if
C
C---- Dust absorption and scattering
      call KAZANKA   (XLM, XLP, N, NOPAC, CO, ISWA, HND, XLMDST, DFDST,
     $                ALBDST, XLDT, ADT, ALBDT, XFRQ, BCKSM, KOPAC)
C
C---- Statistical, Composite, & Averaged line absorption & scattering
      call KOLYMA    (XLM, XLP, N, NOPAC, CO, ISWA, XNE, TE, HND,
     $                WAVEK, ALBK, ALB, ARRK, WAVCO, ARRCO, WAVCA,
     $                ARRCA, BCKSM, KOPAC, CQT, CQA, BMULT, SLO, CLO,
     $                ALO)
C     !END
      call BYE ('SHIMMER')
C
      return
      end
