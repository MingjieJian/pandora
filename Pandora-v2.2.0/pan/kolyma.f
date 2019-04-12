      subroutine KOLYMA
     $(XLM,XLP,N,NOPAC,CO,ISWA,XNE,TE,HND,WAVEK,ALBK,ALB,ARRK,WAVCO,
     $ ARRCO,WAVCA,ARRCA,BCKSM,KOPAC,CQT,CQA,BMULT,SLO,CLO,ALO)
C
C     Rudolf Loeser, 2004 Aug 27
C---- Computes generalized background line absorption and scattering
C     (the Kurucz opacities STATISTICAL or COMPOSITE; and AVERAGED),
C     which share a common albedo table.
C     (This is version 2 of KOLYMA.)
C     !DASH
      save
C     !DASH
      real*8 ALB, ALBK, ALO, ARRCA, ARRCO, ARRK, BCKSM, BMULT, CLO, CO,
     $       CQA, CQT, HND, SLO, TE, WAVCA, WAVCO, WAVEK, XLM, XLP, XNE
      integer IOVER, ISWA, KOPAC, N, NOPAC
      logical AVER, CMPO, DOIT, MUST, NEWAVE, NEWDAT, NEWHND, NEWTE,
     $        NEWXNE, STAT
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
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C     !DASH
      external BULLY, HANSEL, WARTS, FILLY, FOLLY, ZERO1, ZIZIA, BILLY,
     $         GRETEL, HI, BYE
C
C               XNE(N), TE(N), HND(N), ISWA(Nopac), WAVCO(NCP), ALB(N),
      dimension XNE(*), TE(*), HND(*), ISWA(*),     WAVCO(*),   ALB(*),
C
C               CQT(NCQ), WAVEK(KNW), ALBK(N), WAVCA(KWA), CO(Nopac,N),
     $          CQT(*),   WAVEK(*),   ALBK(*), WAVCA(*),   CO(*),
C
C               ARRCO(NCP,N), KOPAC(Nopac), ARRCA(KWA,N), BCKSM(NCSBA),
     $          ARRCO(*),     KOPAC(*),     ARRCA(*),     BCKSM(*),
C
C               CLO(N), CQA(NCQ), SLO(N), ALO(N), ARRK(KNW,N)
     $          CLO(*), CQA(*),   SLO(*), ALO(*), ARRK(*)
C
      call HI ('KOLYMA')
C     !BEG
      NEWAVE = XLM.ne.XLP
      NEWTE  = BCKSM( 1).ne.CSBA( 1)
      NEWXNE = BCKSM( 3).ne.CSBA( 3)
      NEWHND = BCKSM( 4).ne.CSBA( 4)
      MUST   = (IOVER.le.1)
      NEWDAT = MUST.or.NEWAVE.or.NEWTE.or.NEWHND.or.NEWXNE
C
      STAT = (KOPAC(22).gt.0).or.(KOPAC(23).gt.0)
      CMPO = (KOPAC(24).gt.0).or.(KOPAC(25).gt.0)
      AVER = (KOPAC(31).gt.0).or.(KOPAC(32).gt.0)
C
      if((STAT.or.CMPO.or.AVER).and.NEWDAT) then
        call ZERO1   (ALB, N)
        call ZIZIA   (ALB, ALBK, XLM, XNE, HND, TE, CQT, CQA, N)
        if(STAT) then
          call ZERO1 (SLO, N)
        end if
        if(CMPO) then
          call ZERO1 (CLO, N)
        end if
        if(AVER) then
          call ZERO1 (ALO, N)
        end if
      end if
C     !EJECT
      call WARTS    (22, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call FILLY  (22, XLM, N, NOPAC, WAVEK, ARRK, SLO, ALB, BMULT,
     $               CO)
        ISWA(22) = 1
      end if
      call WARTS    (23, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call FOLLY  (23, XLM, N, NOPAC, WAVEK, ARRK, SLO, ALB, BMULT,
     $               CO)
        ISWA(23) = 1
      end if
C
      call WARTS    (24, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call HANSEL (24, XLM, N, NOPAC, WAVCO, ARRCO, CLO, ALB, BMULT,
     $               CO)
        ISWA(24) = 1
      end if
      call WARTS    (25, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call GRETEL (25, XLM, N, NOPAC, WAVCO, ARRCO, CLO, ALB, BMULT,
     $               CO)
        ISWA(25) = 1
      end if
C
      call WARTS    (31, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call BILLY  (31, XLM, N, NOPAC, WAVCA, ARRCA, ALO, ALB, BMULT,
     $               CO)
        ISWA(31) = 1
      end if
      call WARTS    (32, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call BULLY  (32, XLM, N, NOPAC, WAVCA, ARRCA, ALO, ALB, BMULT,
     $               CO)
        ISWA(32) = 1
      end if
C     !END
      call BYE ('KOLYMA')
C
      return
      end
