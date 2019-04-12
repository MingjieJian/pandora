      subroutine HOLE
     $(XLM,XLP,CORE,N,NOPAC,KEMIT,CO,ISWA,CB,ISWE,EMU,XNE,TE,V,VEX,H1,
     $ O3N,O3BD,BCKSM,KOPAC,CABS,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes absorption and emission of background O-III lines.
C     !DASH
      save
C     !DASH
      real*8 BCKSM, CABS, CB, CEMI, CO, CORE, EMU, H1, O3BD, O3N, TE, V,
     $       VEX, XLM, XLP, XNE
      integer IONST, ISWA, ISWE, KEMIT, KOPAC, N, NM, NOPAC
      logical DOIT, DUMP, NEWAVE, NEWBD, NEWDAT, NEWH1, NEWN, NEWTE,
     $        NEWV, NEWXNE, OXYGEN3, lummy
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
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
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
      external HALL, TALL, CORSICA, ZERO1, WARTS, HI, BYE
C
C               KOPAC(Nopac), BCKSM(NCSBA), CABS(N,Nlin), CEMI(N,Nlin),
      dimension KOPAC(*),     BCKSM(*),     CABS(*),      CEMI(*),
C
C               V(N), H1(N), VEX(N), XNE(N), O3BD(N,Limd), ISWA(Nopac),
     $          V(*), H1(*), VEX(*), XNE(*), O3BD(*),      ISWA(*),
C
C               ISWE(Nopac), O3N(N,Limd), CB(Nopac,N), CO(Nopac,N),
     $          ISWE(*),     O3N(*),      CB(*),       CO(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('HOLE')
C     !BEG
      NEWTE  = BCKSM( 1).ne.CSBA( 1)
      NEWV   = BCKSM( 2).ne.CSBA( 2)
      NEWXNE = BCKSM( 3).ne.CSBA( 3)
      NEWN   = BCKSM(52).ne.CSBA(52)
      NEWBD  = BCKSM(53).ne.CSBA(53)
      NEWH1  = BCKSM(43).ne.CSBA(43)
      NEWAVE = XLM.ne.XLP
C
      NEWDAT = NEWTE.or.NEWV.or.NEWXNE.or.NEWN.or.NEWBD.or.NEWH1.or.
     $         NEWAVE
      call WARTS     (45, NEWDAT, KOPAC, CO, NOPAC, N, DOIT )
      call WARTS     (45, NEWDAT, KOPAC, CB, NOPAC, N, lummy)
C
      if(DOIT) then
        NM = N*MX3L
        call ZERO1   (CABS, NM)
        call ZERO1   (CEMI, NM)
C
        OXYGEN3 = (QELSM(:3).eq.'O  ').and.(IONST.eq.3)
C
C----   Background lines opacities
        call HALL    (XLM, XLP, CORE, OXYGEN3, KOPAC, N, XNE, TE, V,
     $                O3N, O3BD, EMU, VEX, H1, BCKSM, ISWA, CABS)
C
        if(KEMIT.gt.0) then
C----     Background lines source functions
          call TALL  (XLM, XLP, CORE, OXYGEN3, KOPAC, ISWA, N, TE,
     $                O3BD, BCKSM, DUMP, ISWE, CEMI)
        end if
C
C----   Final processing and shuffling (? and dump)
        call CORSICA (DUMP, 'Oxygen-III', ISWA, 45, NOPAC, N, CO, CB,
     $                MX3L, CABS, CEMI, KEMIT)
      end if
C     !END
      call BYE ('HOLE')
C
      return
      end
