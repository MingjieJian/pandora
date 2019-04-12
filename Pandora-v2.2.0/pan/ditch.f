      subroutine DITCH
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,H1,
     $ O2N,O2BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes absorption and emission of background O-II lines.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, O2BD, O2N, TE, V,
     $       VEX, XLM, XLP, XNE
      integer IONST, ISWA, ISWE, KEMIT, KOPAC, N, NM, NOPAC
      logical DOIT, DUMP, NEWAVE, NEWBD, NEWDAT, NEWH1, NEWN, NEWTE,
     $        NEWV, NEWXNE, OXYGEN2, lummy
      character QELSM*8
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
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
C     !DASH
C     !EJECT
      external DUCKY, PLUCKY, CORSICA, ZERO1, WARTS, HI, BYE
C
C               KOPAC(Nopac), BCKSM(NCSBA), CABS(N,Nlin), CEMI(N,Nlin),
      dimension KOPAC(*),     BCKSM(*),     CABS(*),      CEMI(*),
C
C               V(N), H1(N), VEX(N), XNE(N), O2BD(N,Limd), ISWA(Nopac),
     $          V(*), H1(*), VEX(*), XNE(*), O2BD(*),      ISWA(*),
C
C               ISWE(Nopac), O2N(N,Limd), CB(Nopac,N), CO(Nopac,N),
     $          ISWE(*),     O2N(*),      CB(*),       CO(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('DITCH')
C     !BEG
      NEWTE  = BCKSM( 1).ne.CSBA( 1)
      NEWV   = BCKSM( 2).ne.CSBA( 2)
      NEWXNE = BCKSM( 3).ne.CSBA( 3)
      NEWN   = BCKSM(49).ne.CSBA(49)
      NEWBD  = BCKSM(50).ne.CSBA(50)
      NEWH1  = BCKSM(43).ne.CSBA(43)
      NEWAVE = XLM.ne.XLP
C
      NEWDAT = NEWTE.or.NEWV.or.NEWXNE.or.NEWN.or.NEWBD.or.NEWH1.or.
     $         NEWAVE
      call WARTS      (44, NEWDAT, KOPAC, CO, NOPAC, N, DOIT )
      call WARTS      (44, NEWDAT, KOPAC, CB, NOPAC, N, lummy)
C
      if(DOIT) then
        NM = N*MX2L
        call ZERO1    (CABS, NM)
        call ZERO1    (CEMI, NM)
C
        OXYGEN2 = (QELSM(:3).eq.'O  ').and.(IONST.eq.2)
C
C----   Background lines opacities
        call DUCKY    (XLM, XLP, CORE, OXYGEN2, KOPAC, N, XNE, TE, V,
     $                 O2N, O2BD, EMU, VEX, H1, BCKSM, ISWA, CABS)
C
        if(KEMIT.gt.0) then
C----     Background lines source functions
          call PLUCKY (XLM, XLP, CORE, OXYGEN2, KOPAC, ISWA, N, TE,
     $                 O2BD, BCKSM, DUMP, ISWE, CEMI)
        end if
C
C----   Final processing and shuffling (? and dump)
        call CORSICA  (DUMP, 'Oxygen-II', ISWA, 44, NOPAC, N, CO, CB,
     $                 MX2L, CABS, CEMI, KEMIT)
      end if
C     !END
      call BYE ('DITCH')
C
      return
      end
