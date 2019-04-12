      subroutine ZUPPY
     $(HND,XNE,H2N,ZHEL,HNDO,XNEO,HNKR,HNIR,H2NR,ZHER,HNDNW,XNENW,
     $ XPBL,R,WEIT,H1)
C
C     Rudolf Loeser, 1980 Jul 22
C---- Sets up final values of number densities and populations,
C     for ZIPPY.
C     (This is version 2 of ZUPPY.)
C     !DASH
      save
C     !DASH
      real*8 H1, H2N, H2NR, HEL, HND, HNDNW, HNDO, HNIR, HNKR, HSEC, R,
     $       WEIT, XNE, XNENW, XNEO, XPBL, ZHEL, ZHER, dummy
      integer KLOG, KMSS, LLPOPK, LLPOPN, MODE, N, jummy
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (RZQ( 14),HSEC )
      equivalence (RZQ( 31),HEL  )
C     !DASH
      external MOVE1, WEITER, CEDAR, POPIO, INDIA, TOPAZ, WENDY, SABOT,
     $         THAMOS, HI, BYE
C
C               XPBL(Lenpbl), HND(N), XNE(N), XNENW(N), R(N), HNDNW(N),
      dimension XPBL(*),      HND(*), XNE(*), XNENW(*), R(*), HNDNW(*),
C
C               HNDO(N), XNEO(N), HNKR(N), HNIR(N,NL), H2N(N), H2NR(N),
     $          HNDO(*), XNEO(*), HNKR(*), HNIR(*),    H2N(*), H2NR(*),
C
C               ZHEL(N), ZHER(N), WEIT(N), H1(N)
     $          ZHEL(*), ZHER(*), WEIT(*), H1(*)
C
      data KLOG,MODE,KMSS /1, 0, 0/
C     !EJECT
C
      call HI ('ZUPPY')
C     !BEG
C---- Final Hydrogen number density
      call MOVE1  (HND, N, HNDNW)
      call WEITER (HND, HNDNW, HNDO, dummy, HSEC, N, KLOG, MODE, KMSS,
     $             qummy, WEIT)
C     Save for iterative summary
      call SABOT  (HND)
C     Continuum Recalculation control
      call WENDY  (HND, 1, N, 4, 'ZUPPY')
C
C---- Final Hydrogen level populations
      call CEDAR  (HND, XPBL(LLPOPK), XPBL(LLPOPN), H2N, ZHEL,
     $             LIMPOP(1), HNKR, HNIR, H2NR, ZHER)
      call POPIO  ('WRITE', jummy, XPBL)
C     Update reserved level-1 population (and Continuum Recalculation)
      call THAMOS (XPBL, H1)
C---- Final remaining non-LTE ions populations
      call INDIA  (HND, HNDO, R, XPBL(LLPOPN), XPBL(LLPOPK), XPBL)
C
C---- Final electron density
      call MOVE1  (XNE, N, XNENW)
      call WEITER (XNE, XNENW, XNEO, dummy, HEL,  N, KLOG, MODE, KMSS,
     $             qummy, WEIT)
C     Save for iterative summary
      call TOPAZ  (XNE, 1)
C     Continuum Recalculation control
      call WENDY  (XNE, 1, N, 3, 'ZUPPY')
C     !END
      call BYE ('ZUPPY')
C
      return
      end
