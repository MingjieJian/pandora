      subroutine RICHARD
     $(X,W,IW,N,XCBL,XPBL,XNE,ZME,ETA,RZM,HNKR,H2NR,DGM,EF,G,HND,H2N,
     $ HNIR,T5,OP5000,Z,GMASIN,TAU5000,H,FF,HNDS,HNDT,MEITER,NEITER,
     $ NTITER,ZHEL,ZHER,HELABD,PMG,XK,DD,EM,FEM,VV,IMG,DUMP,RECZ,
     $ ZTRM)
C
C     Rudolf Loeser, 1980 Nov 07
C     RL/SGK revised Mar 22 2014 
C---- Calculates HND, XNE and TAU5000 using input Z,
C     with HND adjustment, for the H.S.E. calculation.
C     !DASH
      save
C     !DASH
      real*8 C, DD, DGM, EF, EM, ETA, FACTOR, FEM, FF, G, GMASIN, H,
     $       H2N, H2NR, HELABD, HND, HNDS, HNDT, HNIR, HNKR, ONE,
     $       OP5000, PMG, RZM, T5, TAU5000, V, VV, W, WAVE, X, XCBL, XK,
     $       XNE, XPBL, Z, ZHEL, ZHER, ZME, ZTRM
      integer IMG, IW, LLPOPK, LLPOPN, LTYPE, MEITER, N, NEITER, NTITER,
     $        jummy
      logical CONVERG, DUMP, HOK, KILROY, RECZ
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external PLEASE, GISELA, PRAIRIE, PUPPI, PARODY, RIPPLE, WALBIRI,
     $         GUPPI, CEDAR, POPIO, FUZZ, HALT, PEARL, MERCY, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               T5(12), XPBL(Lenpbl), XCBL(Miklen), ZHEL(N), OP5000(N),
      dimension T5(*),  XPBL(*),      XCBL(*),      ZHEL(*), OP5000(*),
C
C               ETA(N,NMT), HELABD(N), HNKR(N), H2NR(N), Z(N), ZHER(N),
     $          ETA(*),     HELABD(*), HNKR(*), H2NR(*), Z(*), ZHER(*),
C
C               G(N), HNIR(N,Limpop(1)), TAU5000(N), DGM(N), GMASIN(N),
     $          G(*), HNIR(*),           TAU5000(*), DGM(*), GMASIN(*),
C
C               ZME(N), H(N), HND(N), HNDS(N), HNDT(N), XNE(N), IMG(N),
     $          ZME(*), H(*), HND(*), HNDS(*), HNDT(*), XNE(*), IMG(*),
C
C               H2N(N), FF(N), RZM(N), EF(N), PMG(N), XK(N), DD(N),
     $          H2N(*), FF(*), RZM(*), EF(N), PMG(*), XK(*), DD(*),
C
C               EM(N), VV(N), ZTRM(N,NMT)
     $          EM(*), VV(*), ZTRM(N,*)
C
      data WAVE,LTYPE /5.D3, 9/
C
      call HI ('RICHARD')
C     !BEG
      MEITER = 0
      NTITER = 1
      KILROY = .true.
      T5(1)  = ONE
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
  100 continue
        if(DUMP) then
C----     Optional printout
          call PUPPI (Z, HND, RZM, ZME, G, H, XPBL(LLPOPK), HNKR, H2N,
     $                H2NR, XPBL(LLPOPN), HNIR, LIMPOP(1))
        end if
C----   Stop if trouble (HOK from PRAIRIE, above)
        if(.not.HOK) then
          write (MSSLIN(1),101)
  101     format('At least one value of H(i) is unacceptable.')
          call HALT  ('RICHARD', 1)
        end if
C
C----   Compute opacity at 5000 Angstroms
        call RIPPLE  (X, W, IW, WAVE, LTYPE, XCBL, KILROY, OP5000,
     $                DUMP)
C----   Compute Tau5000, and check for convergence of Tau5000(REF)
        call FUZZ    (X, W, Z, TAU5000, OP5000, T5, NTITER, CONVERG,
     $                IMG)
C----   Compute new trial hydrogen number density
        call PLEASE  (T5, FF, NTITER, HND, G, H, Z, FACTOR, HNDS,
     $                HNDT, V, C, EM, FEM, OP5000, TAU5000)
        if(DUMP) then
C----     Optional printout
          call GUPPI (NTITER, T5, FACTOR, Z, OP5000, TAU5000, G, H,
     $                HND, XNE, HNDT, V, C, FF)
        end if
C----   Compute new trial electron density
        call WALBIRI (X, W, XNE, ZME, HND, ZHEL, HNKR, ETA, NEITER,
     $                MEITER, DUMP)
C----   If not converged, set up next pass through loop
        if(.not.CONVERG) then
          NTITER = NTITER+1
C----     Recompute Hydrogen populations (for consistency with
C         the new HND)
          call CEDAR (HND, XPBL(LLPOPK), XPBL(LLPOPN), H2N, ZHEL,
     $                LIMPOP(1), HNKR, HNIR, H2NR, ZHER)
C----     Update Data Block in file, for use by opacity calculation
          call POPIO ('WRITE', jummy, XPBL)
C----     Loop back
          goto 100
        end if
      continue
C     !END
      call BYE ('RICHARD')
C
      return
      end
