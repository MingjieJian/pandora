      subroutine TARPON
     $(X,W,IW,KAMB,KVLG,XND,XNK,Z,VM,ZT,HND,TE,XNE,H2N,RHEAB,SA,RKI,
     $ CKI,CQSI,GVL,GVI,KINOUT,HE2SIM,IMG,LU,SHE,HK,H1,HEK,HE1,HE2K,
     $ HE21,VAMB,VBMB,VCMB,VDMB,VP,VH,VE,V1,V2,V3,ZI,Z1,Z2,Z3,ZION,
     $ FMV,RABD,XION,GHL,GHI,GXL,GXI,G1,GVL1,GNVCHK,RND,ALPHA,GX1M,
     $ ZETA,DZZ,DZB,BETA,GRF,SN1V,SNKV,RNDU,PLK,PKL,SPKL,HEND,ZXG,PNF,
     $ PALBET,PBETAL,PBETGM,PGMBET,RHAB,VEC,RGVL,XPBL,DIONL,DLVSL,
     $ SHE2,BETAR,GVO,KZAUG,KZAS,KZUNL)
C
C     Rudolf Loeser, 1987 Oct 05
C---- Computes ambipolar diffusion and velocity gradient terms.
C     XND and XNK may also be adjusted (option AMDN1)!
C     !DASH
      save
C     !DASH
      real*8 ALPHA, BETA, BETAR, CKI, CQSI, DIONL, DLVSL, DZB, DZZ, FMV,
     $       G1, GHI, GHL, GNVCHK, GRF, GVI, GVL, GVL1, GVO, GX1M, GXI,
     $       GXL, H1, H2N, HE1, HE21, HE2K, HEK, HEND, HK, HND, PALBET,
     $       PBETAL, PBETGM, PGMBET, PKL, PLK, PNF, RABD, RGVL, RHAB,
     $       RHEAB, RKI, RND, RNDU, SA, SHE, SHE2, SN1V, SNKV, SPKL, TE,
     $       V1, V2, V3, VAMB, VBMB, VCMB, VDMB, VE, VEC, VH, VM, VP, W,
     $       X, XION, XND, XNE, XNK, XPBL, Z, Z1, Z2, Z3, ZETA, ZI,
     $       ZION, ZT, ZXG
      integer IMG, IQAN1, ITMXN, ITN1R, IW, KAMB, KINOUT, KVLG, KZAS,
     $        KZAUG, KZUNL, LU, LUAP, LUNP, LUVP, MN1, N, NL, NNL
      logical DIDRGVL, DMPA, DMPN, DMPV, DORGVL, HE2SIM
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(272),IQAN1)
C     !EJECT
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
      equivalence (KZQ(157),ITN1R)
C     !DASH
C     !EJECT
      external PONTAR, ALESIA, CONSUL, CLOUNS, GOCKEL, URSUS, PATRON,
     $         ZERO1, CARY, HI, BYE
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
C               SA(N), GNVCHK(N), GHL(N,NL), GHI(N), GXL(N,NL), GXI(N),
     $          SA(*), GNVCHK(*), GHL(*),    GHI(*), GXL(*),    GXI(*),
C
C               V1(N), VP(N), DZZ(N), DZB(N), Z3(N), PKL(N,NL), GRF(N),
     $          V1(*), VP(*), DZZ(*), DZB(*), Z3(*), PKL(*),    GRF(*),
C
C               GX1M(N), G1(N), HK(N), VEC(N), HEK(N), HE1(N), HE2K(N),
     $          GX1M(*), G1(*), HK(*), VEC(*), HEK(*), HE1(*), HE2K(*),
C
C               HE21(N), VBMB(N), VCMB(N), VDMB(N), VE(N), CQSI(N,NSL),
     $          HE21(*), VBMB(*), VCMB(*), VDMB(*), VE(*), CQSI(*),
C
C               V2(N), V3(N), Z1(N), Z2(N), XION(N), ZION(N), RHEAB(N),
     $          V2(*), V3(*), Z1(*), Z2(*), XION(*), ZION(*), RHEAB(*),
C
C               ALPHA(NL), ZETA(N), CVL1(N), RND(N,NL), IMG(N), SHE(N),
     $          ALPHA(*),  ZETA(*), GVL1(*), RND(*),    IMG(*), SHE(*),
C
C               SN1V(N,ITMX), RKI(N,NSL), BETA(N), CKI(N,NSL), SPKL(N),
     $          SN1V(*),      RKI(*),     BETA(*), CKI(*),     SPKL(*),
C
C               FMV(N), RNDU(N,NL), RABD(N), H1(N), HEND(N), PLK(N,NL),
     $          FMV(*), RNDU(*),    RABD(*), H1(*), HEND(*), PLK(*),
C
C               PBETGM(N), RGVL(N,NL), PALBET(N), PBETAL(N), PGMBET(N),
     $          PBETGM(*), RGVL(*),    PALBET(*), PBETAL(*), PGMBET(*),
C
C               XPBL(Lenpbl), SNKV(N,ITMX), ZXG(N), DIONL(N), DLVSL(N),
     $          XPBL(*),      SNKV(*),      ZXG(*), DIONL(*), DLVSL(*),
C
C               SHE2(N), BETAR(N), KZAUG(N), KZUNL(N,IOMX), GVO(N,NL),
     $          SHE2(*), BETAR(*), KZAUG(*), KZUNL(*),      GVO(*),
C
C               RHAB(N), PNF(N), KZAS(N,JTMX)
     $          RHAB(*), PNF(*), KZAS(*)
C     !EJECT
C
      call HI ('TARPON')
C     !BEG
      NNL = N*NL
      call ZERO1  (GHL, NNL)
      call ZERO1  (GHI, N  )
      call ZERO1  (GXL, NNL)
      call ZERO1  (GXI, N  )
C
C---- Compute GX1M based on FMV
      call GOCKEL (N, FMV, GX1M)
C---- Compute rates PKL and PLK
      call ALESIA (N, NL, RKI, CKI, CQSI, XNE, SA, PLK, PKL, SPKL)
C---- Compute ALPHA for GHL
      call CARY   (NL, ALPHA)
C---- Set up logical output units
      call PATRON (LU, LUAP, DMPA, LUVP, DMPV, LUNP, DMPN)
C
      ITMXN = ITN1R
C---- Calculate GV = GH ( ?) + GX ( ?), and "Special N-1" ( ?)
      call PONTAR (X, W, IW, LUAP, DMPA, LUVP, DMPV, LUNP, DMPN, ITMXN,
     $             KAMB, KVLG, XND, XNK, Z, VM, ZT, HND, TE, XNE, H2N,
     $             RHEAB, GVL, GVI, SHE, HK, H1, HEK, HE1, HE2K, HE21,
     $             VAMB, VBMB, VCMB, VDMB, VP, VH, VE, V1, V2, V3, ZI,
     $             Z1, Z2, Z3, ZION, XION, GHL, GHI, GXL, GXI, G1,
     $             GVL1, GNVCHK, RND, ALPHA, GX1M, ZETA, DZZ, DZB,
     $             BETA, GRF, SN1V, SNKV, RNDU, PLK, PKL, SPKL, HEND,
     $             RABD, ZXG, VEC, PNF, PALBET, PBETAL, PBETGM, PGMBET,
     $             RHAB, KINOUT, HE2SIM, DIONL, DLVSL, XPBL, SHE2,
     $             BETAR, GVO, IMG, KZAUG, KZAS, KZUNL)
C
C---- Analysis of GVL
      DORGVL = (IQAN1.gt.0)
      call CLOUNS (LU, DORGVL, N, NL, PLK, GVL, RGVL, DIDRGVL)
C---- Plots
      call CONSUL (LU, LUAP, MN1, N, NL, Z, VE, VP, VH, V1, V2, V3, VM,
     $             ZT, ZI, Z1, Z2, Z3, GVI, GVL, TE, HND, H1, HK, XNE,
     $             HE1, BETA, HE2K, RGVL, DIDRGVL, KAMB, KVLG, W)
C---- Debug checksums
      call URSUS  (MN1, KAMB, KVLG, N, NL, XND, GHI, GHL, GXI, GXL,
     $             GVI, GVL)
C     !END
      call BYE ('TARPON')
C
      return
      end
