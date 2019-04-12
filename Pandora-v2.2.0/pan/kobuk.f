      subroutine KOBUK
     $(W,IW,XLM,XLP,N,NOPAC,ISWA,CO,XNE,TE,XNC,BDHM,V,VXS,XLMXX,XLMDR,
     $ HNK,HN,BDH,VEC,TRM,BCKSM,CON,XLYB,KOPAC)
C
C     Rudolf Loeser, 1988 Feb 04
C---- Supervises calculation of absorption requiring H data.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, BDH, BDHM, CO, CON, HN, HNK, TE, TRM, V, VEC, VXS,
     $       W, XLM, XLMDR, XLMXX, XLP, XLYB, XNC, XNE
      integer ISWA, IW, KOPAC, N, NOPAC
      logical DOIT, NEWAVE, NEWBDHM, NEWCOL, NEWCON, NEWDAT, NEWHN,
     $        NEWHNK, NEWLYM, NEWTE, NEWV, NEWVXS, NEWXNE
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
      external CATFOOT, CATTAIL, RAYSCAT, CATNIP, BUCKLE, HUFF, COOP,
     $         WARTS, HI, BYE
C
      dimension W(*), IW(*)
C
C               CO(Nopac,N), ISWA(Nopac), TE(N), HN(N,Limdat(1)), V(N),
      dimension CO(*),       ISWA(*),     TE(*), HN(*),           V(*),
C
C               XLMXX(LLY), XLMDR(LLY), XNE(N), HNK(N), CON(N), VXS(N),
     $          XLMXX(*),   XLMDR(*),   XNE(*), HNK(*), CON(*), VXS(*),
C
C               KOPAC(Nopac), XLYB(Lenlyb), BDH(N,Limdat(1)), BDHM(N),
     $          KOPAC(*),     XLYB(*),      BDH(*),           BDHM(*),
C
C               BCKSM(NCSBA), XNC(N), VEC(N), TRM(N)
     $          BCKSM(*),     XNC(*), VEC(*), TRM(*)
C
      call HI ('KOBUK')
C     !BEG
      NEWTE   = BCKSM( 1).ne.CSBA( 1)
      NEWV    = BCKSM( 2).ne.CSBA( 2)
      NEWXNE  = BCKSM( 3).ne.CSBA( 3)
      NEWBDHM = BCKSM( 5).ne.CSBA( 5)
      NEWCON  = BCKSM( 8).ne.CSBA( 8)
      NEWHN   = BCKSM( 9).ne.CSBA( 9)
      NEWHNK  = BCKSM(11).ne.CSBA(11)
      NEWVXS  = BCKSM(39).ne.CSBA(39)
      NEWAVE  = XLM.ne.XLP
C
C
C---- H Ly lines
      NEWLYM  = NEWXNE.or.NEWHN.or.NEWTE.or.NEWV.or.NEWAVE
      call BUCKLE (W, IW, NEWLYM, XLM, N, NOPAC, KOPAC, ISWA, TE, XNE,
     $             XNC, V, HN, BDH, XLMXX, XLMDR, CO, VEC, XLYB)
C
C---- CO lines
      NEWCOL  = NEWCON.or.NEWTE.or.NEWV.or.NEWHN.or.NEWVXS.or.NEWAVE
      call COOP   (W, IW, NEWCOL, XLM, N, NOPAC, KOPAC, ISWA, TE, V,
     $             VXS, HN, CON, CO, VEC, TRM)
C     !EJECT
C---- H- bf
      NEWDAT = NEWXNE.or.NEWHN.or.NEWBDHM.or.NEWTE.or.NEWAVE
      call WARTS     ( 1, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call CATFOOT ( 1, XLM, N, NOPAC, XNE, HN, TE, BDHM, CO)
        ISWA( 1) = 1
      end if
C
C---- H- ff
      NEWDAT = NEWXNE.or.NEWHN.or.NEWTE.or.NEWAVE
      call WARTS     ( 2, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call CATTAIL ( 2, XLM, N, NOPAC, XNE, HN, TE, CO)
        ISWA( 2) = 1
      end if
C
C---- H Rayleigh scattering
      NEWDAT = NEWHN.or.NEWAVE
      call WARTS     ( 4, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call RAYSCAT ( 4, XLM, N, NOPAC, HN, CO)
        ISWA( 4) = 1
      end if
C
C---- H2+
      NEWDAT = NEWHNK.or.NEWHN.or.NEWTE.or.NEWAVE
      call WARTS     ( 9, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call CATNIP  ( 9, XLM, N, NOPAC, HNK, HN, TE, CO)
        ISWA( 9) = 1
      end if
C
C---- H ff
      NEWDAT = NEWXNE.or.NEWHNK.or.NEWTE.or.NEWAVE
      call WARTS     (13, NEWDAT, KOPAC, CO, NOPAC, N, DOIT)
      if(DOIT) then
        call HUFF    (13, XLM, N, NOPAC, XNE, HNK, TE, CO)
        ISWA(13) = 1
      end if
C     !END
      call BYE ('KOBUK')
C
      return
      end
