      subroutine HENRY
     $(X,W,IW,N,XCBL,XPBL,XNE,ZME,ETA,RZM,HNKR,H2NR,DGM,EF,G,HND,H2N,
     $ HNIR,H,OP5000,Z,GMASIN,TAU5000,MEITER,NEITER,ZHEL,HELABD,PMG,
     $ XK,DD,EM,FEM,VV,IMG,DUMP,RECZ,ZTRM)
C
C     Rudolf Loeser, 1980 Nov 07
C     Rl/SGK revised Mar 22 2014 
C---- Computes HND, XNE and TAU5000 using input Z,
C     without HND adjustment, for the H.S.E. calculation.
C     !DASH
      save
C     !DASH
      real*8 DD, DGM, EF, EM, ETA, FEM, G, GMASIN, H, H2N, H2NR, HELABD,
     $       HND, HNIR, HNKR, OP5000, PMG, RGV, RMG, RZM, TAU5000, VV,
     $       W, WAVE, X, XCBL, XK, XNE, XPBL, Z, ZHEL, ZME, dummy, ZTRM
      integer IMG, IW, LLPOPK, LLPOPN, LTYPE, MEITER, N, NEITER
      logical DUMP, HOK, KILROY, RECZ
      character qummy*8
C     !COM
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
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external PEARL, GISELA, PRAIRIE, PARODY, RIPPLE, PUPPI, WALBIRI,
     $         MERCY, PEASE, FUZZ, HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), HELABD(N), ZME(N), ETA(N,NMT), OP5000(N),
      dimension XCBL(*),      HELABD(*), ZME(*), ETA(*),     OP5000(*),
C
C               HNKR(N), H2N(N), IMG(N), G(N), HND(N), ZHEL(N), DGM(N),
     $          HNKR(*), H2N(*), IMG(*), G(*), HND(*), ZHEL(*), DGM(*),
C
C               H2NR(N), HNIR(N,Limpop(1)), XNE(N), XPBL(Lenpbl), Z(N),
     $          H2NR(*), HNIR(*),           XNE(*), XPBL(*),      Z(*),
C
C               TAU5000(N), EF(N), PMG(N), RZM(N), GMASIN(N), XK(N),
     $          TAU5000(*), EF(*), PMG(*), RZM(*), GMASIN(*), XK(*),
C
C               H(N), DD(N), EM(N), VV(N), ZTRM(N,NMT)
     $          H(*), DD(*), EM(*), VV(*), ZTRM(N,*)
C
      data WAVE,LTYPE /5.D3, 9/
C
      call HI ('HENRY')
C     !BEG
      MEITER = 0
      KILROY = .true.
C---- Compute ZME
      call PEARL   (X, W, XNE, ZME, ETA, ZTRM)
C---- Compute G
      call GISELA  (N, HELABD, DGM, EF, G)
C---- (? Z-from-mass)
      call PARODY  (X, W, IW, GMASIN, G, Z, N, XCBL, IMG, RECZ, 1)
C---- Compute H
      call PRAIRIE (X, W, G, H, IMG, HOK)
C---- Compute EM (= M, the magnetic pressure term)
      call MERCY   (X, W, N, PMG, H, Z, EF, XK, DD, VV, IMG, EM)
C     !EJECT
      if(DUMP) then
C----   Optional printout
        call PUPPI (Z, HND, RZM, ZME, G, H, XPBL(LLPOPK), HNKR, H2N,
     $              H2NR, XPBL(LLPOPN), HNIR, LIMPOP(1))
      end if
C---- Stop if trouble (HOK from PRAIRIE, above)
      if(.not.HOK) then
        write (MSSLIN(1),100)
  100   format('At least one value of H(i) is unacceptable.')
        call HALT  ('HENRY', 1)
      end if
C
C---- Compute Opacity at 5000 Angstroms
      call RIPPLE  (X, W, IW, WAVE, LTYPE, XCBL, KILROY, OP5000, DUMP)
C---- Compute Tau5000
      call FUZZ    (X, W, Z, TAU5000, OP5000, dummy, 0, qummy, IMG)
C---- Compute new HND
      call PEASE   (HND, G, EM, FEM, H)
C---- Compute new trial electron density
      call WALBIRI (X, W, XNE, ZME, HND, ZHEL, HNKR, ETA, NEITER,
     $              MEITER, DUMP)
C     !END
      call BYE ('HENRY')
C
      return
      end
