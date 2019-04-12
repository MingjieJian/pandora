      subroutine ZIPPY
     $(X,W,IW,HND,H2N,RHEAB,XNE,Z,TE,VT,VM,DGM,PMG,RZM,AEL,ZME,
     $ TAU5000,PEL,PGS,PTU,PEX,PTO,GMASS,GMASIN,TKIN,PREF,ZRN,H1,
     $ XPBL,XCBL)
C
C     Rudolf Loeser, 1980 Nov 07
C     RL/SGK revised Mar 22 2014 
C---- Controls the Hydrostatic Equilibrium calculations,
C     including an optional iterative loop to make Tau5000(Z=0) = 1.
C
C     XPBL is a buffer for the Hydrogen populations data block;
C     XCBL is a buffer for Continuum data blocks.
C
C     (This is version 4 of ZIPPY.)
C     !DASH
      save
C     !DASH
      real*8 AEL, DGM, FEM, GMASIN, GMASS, H1, H2N, HEABD, HND, PEL,
     $       PEX, PGS, PMG, PREF, PTO, PTU, RHEAB, RZM, TAU5000, TE,
     $       TKIN, VM, VT, W, X, XCBL, XNE, XPBL, Z, ZME, ZRN, dummy
      integer IDD, IEF, IEM, IEMN, IETA, IFF, IGD, IGR, IH2NR, IHELA,
     $        IHNDN, IHNDO, IHNDP, IHNDS, IHNDT, IHNEU, IHNIR, IHNKR,
     $        IHP, IIMG, IIPTO, IN, IOP5, IPNH, IRIND, IS, ISWA, IT5,
     $        IVEC, IVET, IVV, IW, IWEIT, IWS, IXK, IXNEN, IXNEO, IYPBL,
     $        IZHEL, IZHER, JN, KASE, KODE, LLPOPK, LLPOPN, MEITER, MO,
     $        MOX, MUX, N, NEITER, NHITER, NLH, NO, NTITER, IZTRM
      logical DUMP, RACZ, RECZ
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 5),NO   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
      equivalence (LENPOP( 1),NLH )
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
C     !DASH
C     !EJECT
      external  RICHARD, BULLET, ZUPPY, MANTIS, POPIO, CYPRESS, THOMAS,
     $          POPUTIL, HENRY, MOVE1, HOLDIN, GHASTLY, ZAPPY, RAVENNA,
     $          CHIRP, MOLTO, FRANK, FRAGA, MAUVE, FUZZY, WGIVE, IGIVE,
     $          HALT, POUT, LUNA, FOOL, ICY, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XPBL(Lenpbl), HND(N), H2N(N), Z(N), TAU5000(N), PTU(N),
      dimension XPBL(*),      HND(*), H2N(*), Z(*), TAU5000(*), PTU(*),
C
C               TE(N), VT(N), RZM(N), AEL(N), RHEAB(N), TKIN(N), VM(N),
     $          TE(*), VT(*), RZM(*), AEL(*), RHEAB(*), TKIN(*), VM(*),
C
C               PEL(N), PGS(N), PTO(N), GMASS(N), XCBL(Miklen), ZME(N),
     $          PEL(*), PGS(*), PTO(*), GMASS(*), XCBL(*),      ZME(*),
C
C               PEX(N), XNE(N), GMASIN(N), PREF(N), ZRN(N), PMG(N),
     $          PEX(*), XNE(*), GMASIN(*), PREF(*), ZRN(*), PMG(*),
C
C               DGM(N), H1(N)
     $          DGM(*), H1(*)
C
      dimension IN(34)
      equivalence
     $(IN( 1),IHNIR ),(IN( 2),IHNKR ),(IN( 3),IETA  ),(IN( 4),IGR   ),
     $(IN( 5),IHP   ),(IN( 6),IOP5  ),(IN( 7),IT5   ),(IN( 8),IHNDO ),
     $(IN( 9),IXNEO ),(IN(10),IHNDN ),(IN(11),IXNEN ),(IN(12),IGD   ),
     $(IN(13),IVEC  ),(IN(14),IRIND ),(IN(15),IHNDS ),(IN(16),IHNDT ),
     $(IN(17),IFF   ),(IN(18),IVV   ),(IN(19),IH2NR ),(IN(20),IZHEL ),
     $(IN(21),IZHER ),(IN(22),IHNDP ),(IN(23),IHNEU ),(IN(24),IHELA ),
     $(IN(25),IEMN  ),(IN(26),IWEIT ),(IN(27),IYPBL ),(IN(28),IVET  ),
     $(IN(29),IPNH  ),(IN(30),IEF   ),(IN(31),IXK   ),(IN(32),IDD   ),
     $(IN(33),IEM   ),(IN(34),IZTRM )
C
      dimension JN(3)
      equivalence
     $(JN( 1),ISWA  ),(JN( 2),IIMG  ),(JN( 3),IIPTO )
C
      call HI ('ZIPPY')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ICY   (IN, IS,  MOX, 'ZIPPY')
      call FUZZY (JN, IWS, MUX, 'ZIPPY')
C     !EJECT
C---- Save current HND and XNE
      call MOVE1     (HND, N, W(IHNDO))
      call MOVE1     (XNE, N, W(IXNEO))
C---- Initialize
      call MOLTO     (DUMP)
      call MAUVE     (KASE)
C---- Set up Helium abundance
      call FRANK     (   'HE ', 0, HEABD, dummy, dummy, dummy, KODE)
      call LUNA      (X, 'HE ',    HEABD, W(IHELA))
C---- Get Helium electrons, and ratio
      call CHIRP     (W, XPBL, W(IZHEL))
C---- Get Hydrogen population data block
      call POPIO     ('ASSURE', 1, XPBL)
C---- Compute pressures, gas density (? and Mach number)
      call GHASTLY   (N, PEL, PGS, PTU, PEX, PMG, W(IPNH), PTO, XNE,
     $                TE, HND, W(IHELA), H2N, VT, VM, W(IGD), W(IEMN))
C---- Initialize Hydrogen population ratios
      call CYPRESS   (HND, XPBL(LLPOPK), XPBL(LLPOPN), H2N, W(IZHEL),
     $                W(IPNH), LIMPOP(1), W(IHNKR), W(IHNIR), W(IH2NR),
     $                W(IZHER), W(IEF))
C---- Compute new HND and XNE, and TAU5000
      if(KASE.eq.1) then
C       Using input TAUK values
        call THOMAS  (X, W, IW, N, XCBL, XPBL, HND, H2N, TKIN,
     $                W(IHNKR), W(IH2NR), W(IHNIR), W(IGR), W(IHP),
     $                W(IEF), W(IHNDS), XNE, ZME, W(IETA), W(IOP5),
     $                TAU5000, MEITER, NEITER, NHITER, W(IZHEL),
     $                W(IZHER), W(IHNDP), W(IHELA), Z, PMG, W(IXK),
     $                W(IDD), W(IEM), W(IVV), IW(IIMG), DUMP, 
     $                W(IZTRM))
      else if(KASE.eq.2) then
C       Using input Z values, with HND adjustment
        call RICHARD (X, W, IW, N, XCBL, XPBL, XNE, ZME, W(IETA),
     $                RZM, W(IHNKR), W(IH2NR), DGM, W(IEF), W(IGR),
     $                HND, H2N, W(IHNIR), W(IT5), W(IOP5), Z, GMASIN,
     $                TAU5000, W(IHP), W(IFF), W(IHNDS), W(IHNDT),
     $                MEITER, NEITER, NTITER, W(IZHEL), W(IZHER),
     $                W(IHELA), PMG, W(IXK), W(IDD), W(IEM), FEM,
     $                W(IVV), IW(IIMG), DUMP, RACZ, W(IZTRM))
      else if(KASE.eq.3) then
C       Using input Z values, without HND adjustment
        call HENRY   (X, W, IW, N, XCBL, XPBL, XNE, ZME, W(IETA), RZM,
     $                W(IHNKR), W(IH2NR), DGM, W(IEF), W(IGR), HND,
     $                H2N, W(IHNIR), W(IHP), W(IOP5), Z, GMASIN,
     $                TAU5000, MEITER, NEITER, W(IZHEL), W(IHELA),
     $                PMG, W(IXK), W(IDD), W(IEM), FEM, W(IVV),
     $                IW(IIMG), DUMP, RACZ, W(IZTRM))
      else
        write (MSSLIN(1),100) KASE
  100   format('KASE =',I12,', which is not 1, 2 or 3.')
        call HALT    ('ZIPPY', 1)
      end if
C     !EJECT
C---- Set up final values of weighted HND and XNE, recompute all
C     non-LTE ion populations to remain consistent with new HND,
C     and update all population data blocks in file.
      call ZUPPY   (HND, XNE, H2N, W(IZHEL), W(IHNDO), W(IXNEO),
     $              W(IHNKR), W(IHNIR), W(IH2NR), W(IZHER), W(IHNDN),
     $              W(IXNEN), XPBL, W(IRIND), W(IWEIT), H1)
C---- Recompute XNC
      call FRAGA   (X, W(IYPBL))
C---- Update pressures, gas density (? and Mach number)
      call GHASTLY (N, PEL, PGS, PTU, PEX, PMG, W(IPNH), PTO, XNE,
     $              TE, HND, W(IHELA), H2N, VT, VM, W(IGD), W(IEMN))
      call HOLDIN  (N, PTO, PMG, GMASS)
C---- Printout
      call POPUTIL (XPBL, 1, 0, dummy, 0, dummy, 1, W(IHNEU), 0,
     $              dummy)
      call ZAPPY   (N, Z, TKIN, W(IHNEU), HND, XNE, PGS, PTU, PTO,
     $              TAU5000, GMASS, TE, VT, VM, DGM, PMG, W(IHNDO),
     $              W(IXNEO), W(IGR), W(IHP), W(IHNDN), W(IXNEN),
     $              W(IGD), W(IT5), W(IFF), W(IRIND), W(IHELA), XCBL,
     $              NTITER, NEITER, MEITER, NHITER, KASE, NLH,
     $              LIMPOP(1), XPBL(LLPOPK), XPBL(LLPOPN), IW(ISWA),
     $              W(IVEC), W(IVET), IW(IIPTO), PEX, W(IEMN),
     $              W(IEF), W(IXK), W(IDD), W(IEM), FEM)
C---- Recompute Z (if an input MASS table exists)
      call RAVENNA (X, W, Z, GMASIN, GMASS, RECZ)
C---- Recompute Z (if an input TAUK table exists)
      call FOOL    (X, W, IW, IW(ISWA), IW(IIMG), MO, RECZ)
C---- Print and plot electrons details
      call BULLET  (X, W, IW, RZM, HND, XNE, W(IZHEL), ZME, AEL,
     $              XPBL(LLPOPK), Z, 1)
C---- Debug checksums
      call MANTIS  (N, HND, XNE, W(IGD), PEL, PGS, PTU, PTO, GMASS, Z)
C---- Recompute HND-dependent quantities ( ? and Z-dependent stuff)
      call POUT    (X, W, IW, (RACZ.or.RECZ))
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'ZIPPY')
      call IGIVE   (IW, 'ZIPPY')
C     !END
      call BYE ('ZIPPY')
C
      return
      end
