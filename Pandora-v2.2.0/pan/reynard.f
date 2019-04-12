      subroutine REYNARD
     $(LUMR,TDST,Z,HE304,CQOUT,FION,FLVS,XJBN,XK,H2N,CON,VAMB,VBMB,
     $ VCMB,VDMB,VH,VP,VE,V1,V2,V3,VM,TE,XRK,XRL,RHEAB,TR,PALBET,
     $ PBETAL,XNE,HND,V,VT,BDHM,PBETGM,PGMBET,GVL,DIONL,DLVSL,VR,
     $ PGS,GMASS,BDI,TCO)
C     Rudolf Loeser, 1980 Jan 04
C---- Puts miscellaneous data into restart file.
C     !DASH
      save
C     !DASH
      real*8 BDHM, BDI, CON, CQOUT, DIONL, DLVSL, FION, FLVS, GMASS,
     $       GVL, H2N, HE304, HND, PALBET, PBETAL, PBETGM, PGMBET, PGS,
     $       RHEAB, TCO, TDST, TE, TR, V, V1, V2, V3, VAMB, VBMB, VCMB,
     $       VDMB, VE, VH, VM, VP, VR, VT, XJBN, XK, XNE, XRK, XRL, Z
      integer IQAMD, IQAN1, IQJLY, IQLYM, IQUTR, IQVLG, IRPUN, J304S,
     $        JHEAB, JYDRO, KSHEL, LUMR, MCXK, MDFG, MDFV, MFONT, MKURU,
     $        MTREF, NOION
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 63),IRPUN)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ(106),MDFV )
      equivalence (KZQ(172),MDFG )
      equivalence (KZQ(174),MKURU)
      equivalence (KZQ(176),MTREF)
      equivalence (KZQ( 15),MFONT)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(23),J304S)
      equivalence (LEST( 1),KSHEL)
      equivalence (LEST(55),MCXK )
      equivalence (LEST(59),JHEAB)
C     !EJECT
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
      equivalence (IUPOP( 1),JYDRO)
C     !EJECT
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
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ(215),IQJLY)
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ(272),IQAN1)
      equivalence (IQQ(221),IQVLG)
C     !DASH
C     !EJECT
      external  YENA, DRAYNE, NARY, RENDY, RAREY, DRYER, DERNAY, RENAD,
     $          DYNAR, REANY, YERDA, NARREDY, HI, BYE
C
C               TDST(N), VAMB(N), HE304(N), CQOUT(N), FION(N), FLVS(N),
      dimension TDST(*), VAMB(*), HE304(*), CQOUT(*), FION(*), FLVS(*),
C
C               XJBN(N,KK), XK(KK), H2N(N), CON(N), VBMB(N), TR(N,NSL),
     $          XJBN(*),    XK(*),  H2N(*), CON(*), VBMB(*), TR(*),
C
C               VCMB(N), VDMB(N), XRK(N,NPQLM), XRL(N,NPQLM), RHEAB(N),
     $          VCMB(*), VDMB(*), XRK(*),       XRL(*),       RHEAB(*),
C
C               V3(N), TE(N), VM(N), VH(N), VP(N), VE(N), V1(N), V2(N),
     $          V3(*), TE(*), VM(*), VH(*), VP(*), VE(*), V1(*), V2(*),
C
C               PALBET(N), PBETAL(N), XNE(N), HND(N), VT(N), PGMBET(N),
     $          PALBET(*), PBETAL(*), XNE(*), HND(*), VT(*), PGMBET(*),
C
C               VR(N), Z(N), PBETGM(N), BDI(N,NL), GVL(N,NL), DIONL(N),
     $          VR(*), Z(*), PBETGM(*), BDI(*),    GVL(*),    DIONL(*),
C
C               DLVSL(N), BDHM(N), V(N), PGS(N), GMASS(N), TCO(N)
     $          DLVSL(*), BDHM(*), V(*), PGS(*), GMASS(*), TCO(*)
C
      call HI ('REYNARD')
C     !BEG
C---- Miscellaneous items
      call YERDA       (LUMR, TDST, H2N, CON, TCO)
C
      if((JYDRO.gt.0).and.(MFONT.gt.0)) then
C----   Stuff for Juan Fontenla
        call DERNAY    (LUMR, Z, TE, XNE, HND, V, VT, BDHM, VM)
      end if
C     !EJECT
      if(NOION.le.0) then
        if(J304S.gt.0) then
C----     Helium line
          call DRAYNE  (LUMR, Z, HE304)
        end if
        if(KSHEL.gt.0) then
C----     K-Shell data
          call NARY    (LUMR, Z, CQOUT)
        end if
        if(IRPUN.gt.0) then
C----     RABD-calculation data
          call RENDY   (LUMR, Z, FLVS, FION, DIONL, DLVSL)
        end if
        if((IQLYM.gt.0).and.(IQJLY.gt.0)) then
C----     Lyman-transfer JNU data
          call RAREY   (LUMR, XK, XJBN)
        end if
        if((IQUTR.le.0).and.(MTREF.eq.1)) then
C----     Calculated TR-effective
          call DRYER   (LUMR, Z, TR)
        end if
        if(IQAMD.gt.0) then
          if((MDFV.gt.0).or.(MDFG.gt.0)) then
C----       Diffusion velocities
            call YENA  (LUMR, Z, TE, VM, VAMB, VBMB, VCMB, VDMB,
     $                  VH, VP, VE, V1, V2, V3, GVL)
          end if
        end if
        if((IQAMD.gt.0).or.(IQAN1.gt.0)) then
          if(JHEAB.gt.0) then
C----       Helium abundance variation
            call RENAD (LUMR, Z, RHEAB)
          end if
        end if
        if((IQAN1.gt.0).and.((IQAMD.gt.0).or.(IQVLG.gt.0))) then
C----     He terms
          call DYNAR   (LUMR, Z, PALBET, PBETAL, PBETGM, PGMBET)
        end if
        if(MCXK.gt.0) then
C----     Charge exchange terms (to be used in Hydrogen runs)
          call REANY   (LUMR, Z, XRK, XRL)
        end if
        if(MKURU.gt.0) then
C----     Stuff for Kurucz's spectrum calculations
          call NARREDY (LUMR, GMASS, TE, V, VR, VM, PGS, XNE,
     $                  BDHM, BDI)
        end if
      end if
C     !END
      call BYE ('REYNARD')
C
      return
      end
