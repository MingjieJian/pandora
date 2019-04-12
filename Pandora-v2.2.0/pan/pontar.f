      subroutine PONTAR
     $(X,W,IW,LUAP,DMPA,LUVP,DMPV,LUNP,DMPN,ITN1R,KAMB,KVLG,XND,XNK,
     $ Z,VM,ZT,HND,TE,XNE,H2N,RHEAB,GVL,GVI,SHE,HK,H1,HEK,HE1,HE2K,
     $ HE21,VAMB,VBMB,VCMB,VDMB,VP,VH,VE,V1,V2,V3,ZI,Z1,Z2,Z3,ZION,
     $ XION,GHL,GHI,GXL,GXI,G1,GVL1,GNVCHK,RND,ALPHA,GX1M,ZETA,DZZ,
     $ DZB,BETA,GRF,SN1V,SNKV,RNDU,PLK,PKL,SPKL,HEND,RABD,ZXG,VEC,
     $ PNF,PALBET,PBETAL,PBETGM,PGMBET,RHAB,KINOUT,HE2SIM,DIONL,
     $ DLVSL,XPBL,SHE2,BETAR,GVO,IMG,KZAUG,KZAS,KZUNL)
C
C     Rudolf Loeser, 1998 Mar 13
C---- Controls the core of Special-N1, AMDIFF & VELGRAD calculations,
C     for TARPON.
C     !DASH
      save
C     !DASH
      real*8 ALPHA, BETA, BETAR, DIONL, DLVSL, DZB, DZZ, G1, GHI, GHL,
     $       GNVCHK, GRF, GVI, GVL, GVL1, GVO, GX1M, GXI, GXL, H1, H2N,
     $       HE1, HE21, HE2K, HEK, HEND, HK, HND, PALBET, PBETAL,
     $       PBETGM, PGMBET, PKL, PLK, PNF, RABD, RHAB, RHEAB, RND,
     $       RNDU, SHE, SHE2, SN1V, SNKV, SPKL, TE, V1, V2, V3, VAMB,
     $       VBMB, VCMB, VDMB, VE, VEC, VH, VM, VP, W, X, XION, XND,
     $       XNE, XNK, XPBL, Z, Z1, Z2, Z3, ZETA, ZI, ZION, ZT, ZXG
      integer IMG, ITN1R, IW, KAMB, KINOUT, KODE, KVLG, KZAS, KZAUG,
     $        KZUNL, LU, LUAP, LUNP, LUVP, MN1, MNG1, N, NL
      logical DIDG1, DMPA, DMPN, DMPV, DOSN1, DUMP, HE2SIM, WONE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(101),MN1  )
      equivalence (KZQ(110),MNG1 )
C     !DASH
C     !EJECT
      external  MAZAME, OLEOSA, COLUGO, AUBREY, CUNINA, PRATON, RAPTON,
     $          CARAMBA, CAROOM, KADI, HI, BYE
      intrinsic max
C
      dimension X(*), W(*), IW(*)
C
C               ITMX = ITN1R+1           JTMX = ITN1R*ITKZA+1
C
C               XND(N,NL), XNK(N), Z(N), VM(N), ZT(N), HND(N), VAMB(N),
      dimension XND(*),    XNK(*), Z(*), VM(*), ZT(*), HND(*), VAMB(*),
C
C               XNE(N), H2N(N), GVL(N,NL), GVI(N), TE(N), ZI(N), VH(N),
     $          XNE(*), H2N(*), GVL(*),    GVI(*), TE(*), ZI(*), VH(*),
C
C               H1(N), GNVCHK(N), GHL(N,NL), GHI(N), GXL(N,NL), GXI(N),
     $          H1(*), GNVCHK(*), GHL(*),    GHI(*), GXL(*),    GXI(*),
C
C               V1(N), VP(N), DZZ(N), DZB(N), Z3(N), PKL(N,NL), PNF(N),
     $          V1(*), VP(*), DZZ(*), DZB(*), Z3(*), PKL(*),    PNF(*),
C
C               GX1M(N), G1(N), HK(N), ZXG(N), HEK(N), HE1(N), HE2K(N),
     $          GX1M(*), G1(*), HK(*), ZXG(*), HEK(*), HE1(*), HE2K(*),
C
C               HE21(N), VBMB(N), VCMB(N), VDMB(N), HEND(N), PLK(N,NL),
     $          HE21(*), VBMB(*), VCMB(*), VDMB(*), HEND(*), PLK(*),
C
C               V2(N), V3(N), Z1(N), Z2(N), XION(N), ZION(N), RHEAB(N),
     $          V2(*), V3(*), Z1(*), Z2(*), XION(*), ZION(*), RHEAB(*),
C
C               ALPHA(NL), ZETA(N), CVL1(N), RND(N,NL), IMG(N), SHE(N),
     $          ALPHA(*),  ZETA(*), GVL1(*), RND(*),    IMG(*), SHE(*),
C
C               SN1V(N,ITMX), BETA(N), SPKL(N), RABD(N), GRF(N), VE(N),
     $          SN1V(*),      BETA(*), SPKL(*), RABD(*), GRF(*), VE(*),
C
C               RNDU(N,NL), PBETGM(N), PALBET(N), PBETAL(N), PGMBET(N),
     $          RNDU(*),    PBETGM(*), PALBET(*), PBETAL(*), PGMBET(*),
C
C               XPBL(Lenpbl), SNKV(N,ITMX), VEC(N), DIONL(N), DLVSL(N),
     $          XPBL(*),      SNKV(*),      VEC(*), DIONL(*), DLVSL(*),
C
C               RHAB(N), SHE2(N), BETAR(N), GVO(N,NL), KZUNL(N,IOMX),
     $          RHAB(*), SHE2(*), BETAR(*), GVO(*),    KZUNL(*),
C
C               KZAUG(N), KZAS(N,JTMX)
     $          KZAUG(*), KZAS(*)
C
C
C     !DASH
C
C            Logical unit numbers and switches for output
C
C     LUAP :  AMBPRNT  = on ;           DMPA :  AMDDMP  = on ;
C     LUVP :  VLGPRNT  = on ;           DMPV :  VELGDMP = on ;
C     LUNP :  ADN1PRNT = on ;           DMPN :  ADN1DMP = on :
C     LU   :  LUAP or LUVP or LUNP ;    DUMP :  DMPA or DMPV or DMPN
C     !EJECT
C
      call HI ('PONTAR')
C     !BEG
      LU   = max(LUAP,LUVP,LUNP)
      DUMP = DMPA.or.DMPV.or.DMPN
C
C---- Compute RND
      call MAZAME    (N, NL, Z, XND, RND, VEC, RNDU, KODE, W, IW)
C---- Compute intermediates
      call AUBREY    (N, Z, BETA, DZB, GRF, WONE, HEND, BETAR)
C---- Print
      call PRATON    (DUMP, N, NL, KODE, HND, H1, HK, RHAB, HEND, HE1,
     $                HEK, BETA, HE21, HE2K, RHEAB, PNF, RNDU, RND,
     $                PLK, PKL, SPKL, BETAR, DZB, SHE, SHE2)
C
      DIDG1 = .false.
      call CAROOM    (DOSN1)
      if(DOSN1) then
C----   Recompute Level-1 (and higher) and continuum populations, and
C       renormalize NK and all of XND
        call CARAMBA (X, W, IW, KAMB, KVLG, ITN1R, LUNP, DMPN, MN1,
     $                N, NL, XNK, XND, RND, G1, Z, ZT, HND, ZI, ZION,
     $                XION, TE, XNE, H2N, RHEAB, VM, PLK, SPKL, HK,
     $                H1, HEK, HE1, HE2K, HE21, BETA, IMG, DZB, SN1V,
     $                SNKV, VEC, RABD, HEND, SHE, SHE2, VAMB, VBMB,
     $                VCMB, VDMB, PNF, PALBET, PBETAL, PBETGM, PGMBET,
     $                RHAB, BETAR, XPBL, KINOUT, HE2SIM, DIDG1, KZAUG,
     $                KZAS, KZUNL)
C----   Compute DIONL and DLVSL, for RABD calulation (CENSUS)
        call RAPTON  (N, KAMB, KVLG, HEND, HEK, SHE, HE2K, SHE2, DIONL,
     $                DLVSL)
      end if
      if(KAMB.gt.0) then
C----   Compute ambipolar diffusion terms
        call COLUGO  (X, W, IW, KAMB, KVLG, LUAP, DMPA, NL, XNK, XND,
     $                GHI, GHL, N, Z, TE, ZT, XNE, HND, H2N, HEND,
     $                RHEAB, HK, H1, HEK, HE1, HE2K, HE21, BETA, SHE,
     $                ZI, ZION, XION, Z1, Z2, Z3, ZXG, VAMB, VBMB,
     $                VCMB, VDMB, VE, VP, VH, V1, V2, V3, VM, ALPHA)
      end if
      if(KVLG.gt.0) then
C----   Compute mass motion terms
        call CUNINA  (X, W, IW, KVLG, LUVP, DMPV, NL, XNK, XND, GXI,
     $                GXL, N, Z, VM, GX1M, DZZ, VBMB, HND, HEND, BETA,
     $                HE1, HE2K, ZETA)
      end if
C---- Compute "Total terms" (and print ?)
      call OLEOSA    (GHI, GXI, XNK, GVI, GHL, GXL, XND, GVL, GVL1, N,
     $                MN1, NL, DUMP)
C---- Fiddle with GVL and GVI
      call KADI      (LU, LUNP, N, MN1, MNG1, NL, Z, GVL1, G1, DIDG1,
     $                GNVCHK, GVL, GVI, GRF, WONE, GVO, W, IW)
C     !END
      call BYE ('PONTAR')
C
      return
      end
