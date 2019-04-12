      subroutine AMBER
     $(X,W,XPBL,H2N,DONE,LU)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Computes and prints Molecular Hydrogen number density,
C     and Atomic Hydrogen abundance correction factor.
C     Upon return, DONE=.true. only if H2N was actually computed.
C     (This is version 4 of AMBER.)
C     !DASH
      save
C     !DASH
      real*8 CEQMX, H2N, W, X, XPBL
      integer IN, IRNH1, IRSH, IS, JJCEQ, JJCHN, JJHND, JJOHN, JJRAB,
     $        JJTE, JYDRO, KRAB, LLPOPK, LLPOPN, LU, MOX, N, NH2CS,
     $        NHTSW
      logical DONE, H2RAYL, H2ZERO, HYDROG
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(139),JJCEQ)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(268),JJCHN)
      equivalence (IZOQ(269),JJOHN)
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
      equivalence (KZQ( 55),NHTSW)
      equivalence (RZQ( 75),CEQMX)
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
      equivalence (LEST( 9),NH2CS)
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
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
C     !EJECT
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
C     !EJECT
      external NAUGHTD, JUAN, POPIO, RESIN, GOMEZ, WENDY, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl), H2N(N)
      dimension XPBL(*),      H2N(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IRSH  ),(IN( 2),IRNH1 )
C
      call HI ('AMBER')
C     !BEG
      DONE = .false.
C---- Computations must be done under two circumstances:
C     1) If this is a hydrogen population update run and if the H2
C        calculation was selected, then both the H2 number densities
C        and the H abundance corrections must be computed
      HYDROG = (NH2CS.gt.0).and.(JYDRO.gt.0)
      KRAB = 1
C     2) If H2 Rayleigh scattering opacity is on, and if the H2 method
C        selector is not zero, then H2 number densities must be computed
C        if they still don't exist yet; moreover, if HYDROG is off,
C        then the H abundance corrections need not be computed
      call NAUGHTD (H2N, 1, N, H2ZERO)
      H2RAYL = (KOPAC(28).gt.0).and.(NHTSW.gt.0).and.H2ZERO
      if(H2RAYL.and.(.not.HYDROG)) then
        KRAB = 0
      end if
C
      if(HYDROG.or.H2RAYL) then
C       (Get, and allocate, W allotment)
        call JUAN  (IN, IS, MOX, 'AMBER')
C
C       (assume that this buffer (XPBL) has already been used)
        call POPIO ('ASSURE', 1, XPBL)
C----   Compute
        call RESIN (X(JJCEQ), X(JJHND), XPBL(LLPOPK), XPBL(LLPOPN),
     $              LIMPOP(1), X(JJCHN), X(JJOHN), W(IRSH), W(IRNH1),
     $              H2N, X(JJRAB), KRAB)
        DONE = .true.
C----   Print
        call GOMEZ (LU, N, X(JJTE), NHTSW, X(JJCEQ), CEQMX, W(IRSH),
     $              W(IRNH1), H2N, X(JJHND), XPBL(LLPOPN), X(JJRAB),
     $              KRAB)
C----   Continuum Recalculation control
        call WENDY (H2N, 1, N, 7, 'AMBER')
C
C       (Give back W allotment)
        call WGIVE (W, 'AMBER')
      end if
C     !END
      call BYE ('AMBER')
C
      return
      end
