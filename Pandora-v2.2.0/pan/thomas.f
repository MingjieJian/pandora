      subroutine THOMAS
     $(X,W,IW,N,XCBL,XPBL,HND,H2N,TKIN,HNKR,H2NR,HNIR,R,P,EF,HNDS,XNE,
     $ ZME,ETA,OP5000,TAU5000,MEITER,NEITER,NHITER,ZHEL,ZHER,HNDP,
     $ HELABD,Z,PMG,XK,DD,EM,VV,IMG,DUMP,ZTRM)
C
C     Rudolf Loeser, 1980 Nov 07
C     RL/SGK revised Mar 22 2014 
C---- Computes HND, XNE and TAU5000 using input TAUK, for the
C     H.S.E. calculation.
C     !DASH
      save
C     !DASH
      real*8 DD, EF, EIDIF, EM, ETA, H2N, H2NR, HELABD, HND, HNDP, HNDS,
     $       HNIR, HNKR, OP5000, P, PMG, R, TAU5000, TKIN, VV, W, WAVE,
     $       X, XCBL, XK, XNE, XPBL, Z, ZHEL, ZHER, ZME, ZTRM
      integer IMG, ITMX, ITYPE, IW, LLPOPK, LLPOPN, MEITER, N, NEITER,
     $        NHITER, jummy
      logical CONVERG, DUMP, KILROY, lummy1, lummy2
      character LABEL*100
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 57),EIDIF)
C     !DASH
      external RIPPLE, MARBLE, PEARL, DERIV1, EFFENDI, WALBIRI, DISMAL,
     $         POPIO, BLEND, CEDAR, ZERO1, MASAI, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XPBL(Lenpbl), XCBL(Miklen), OP5000(N), P(N), HELABD(N),
      dimension XPBL(*),      XCBL(*),      OP5000(*), P(*), HELABD(*),
C
C               TAU5000(N), HNKR(N), H2N(N), HNIR(N,Limpop(1)), IMG(N),
     $          TAU5000(*), HNKR(*), H2N(*), HNIR(*),           IMG(*),
C
C               ZHER(N), H2NR(N), HNDS(N), XNE(N), ETA(N,NMT), HNDP(N),
     $          ZHER(*), H2NR(*), HNDS(*), XNE(*), ETA(*),     HNDP(*),
C
C               HND(N), ZME(N), PMG(N), EF(N), TKIN(N), ZHEL(N), XK(N),
     $          HND(*), ZME(*), PMG(*), EF(*), TKIN(*), ZHEL(*), XK(*),
C
C               R(N), DD(N), EM(N), VV(N), Z(N), ZTRM(N,NTM)
     $          R(*), DD(*), EM(*), VV(*), Z(*), ZTRM(N,*)
C
      data LABEL /'Tau5000'/
      data WAVE  /5.D3/
      data ITMX  /50/
      data ITYPE /9/
C
      call HI ('THOMAS')
C     !BEG
C---- Initialize
      MEITER = 0
      NHITER = 1
      KILROY = .true.
C
C---- Compute DD, d(PMG)/d(z)
      call DERIV1 (Z, PMG, DD, N)
C     !EJECT
  100 continue
C----   Recompute PREF
        call BLEND     (X, W, IW, DUMP)
C----   Compute P
        call MARBLE    (X, W, HND, HELABD, DD, R, P, IMG)
C----   Compute ZME
        call PEARL     (X, W, XNE, ZME, ETA, ZTRM)
C----   Compute new trial hydrogen number density
        call MASAI     (N, EF, P, HNDS, HNDP, HND, EIDIF, CONVERG)
        if(DUMP) then
C----     Optional Printout
          call EFFENDI (NHITER, TKIN, R, P, ZME, EF, HNDP, HND)
        end if
C----   Compute new trial electron density
        call WALBIRI   (X, W, XNE, ZME, HND, ZHEL, HNKR, ETA, NEITER,
     $                  MEITER, DUMP)
C----   Recompute Hydrogen populations (for consistency with new HND)
        call CEDAR     (HND, XPBL(LLPOPK), XPBL(LLPOPN), H2N, ZHEL,
     $                  LIMPOP(1), HNKR, HNIR, H2NR, ZHER)
C----   Update Data Block in file, for use by opacity calculations
        call POPIO     ('WRITE', jummy, XPBL)
C----   If not converged, loop back
        if(.not.CONVERG) then
          NHITER = NHITER+1
          if(NHITER.le.ITMX) then
C           Loop back
            goto 100
          end if
        end if
      continue
C
C---- Compute opacity at 5000 Angstroms
      call RIPPLE      (X, W, IW, WAVE, ITYPE, XCBL, KILROY, OP5000,
     $                  DUMP)
C---- Compute TAU5000
      call DISMAL      (X, W, 1, N, OP5000, TAU5000, LABEL, jummy,
     $                  lummy1, lummy2, IMG)
C
C---- Set unused quantities = 0
      call ZERO1       (XK, N)
      call ZERO1       (EM, N)
C     !END
      call BYE ('THOMAS')
C
      return
      end
