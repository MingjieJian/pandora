      subroutine BUZZARD
     $(N,NL,NLB,TE,XNU,XNUK,BATAL)
C
C     Rudolf Loeser, 1982 Mar 22
C---- Computes Stimulated Emission factors, for ROVER.
C     (This is version 2 of BUZZARD.)
C     !DASH
      save
C     !DASH
      real*8 BATAL, TE, XNU, XNUK
      integer JPOP, N, NL, NLB
C     !COM
C---- IONDATA     as of 2007 Jan 12
C     Data tables for the built-in models of "non-LTE" ions; these
C     models are used to compute bound-free absorption and emission.
C     Hydrogen (of course!) is treated as a special case.
      integer     NPION,NIONL,NDPNT
C
      parameter   (NPION=14)
C     NPION     = number of ion models, as follows:
C     01: H      02: C-I    03: Si-I   04: He-I   05: He-II  06: Al-I
C     07: Mg-I   08: Fe-I   09: Na-I   10: Ca-I   11: O-I    12: S-I
C     13: O-II   14: O-III
C
      parameter   (NIONL=15)
C     NIONL     = maximum number of levels in each model
C
      parameter   (NDPNT=150)
C     NDPNT     = maximum length of tables specifying the wavelength-
C     dependence of the absorption in each continuum, by
C     piece-wise linear approximation.
C
C     REMEMBER to recompile all users when changing NPION, NIONL, NDPNT.
C
      real*8      PILEVL,XLMTHR,CCPLEV,SCPLEV,XLMTAB,RCPTAB
      integer     LIMDAT,MAXDATL,LEND,NPTABL
      character   LLABEL*16
      logical     LLPRNT
      dimension   LIMDAT(            NPION), LLPRNT(            NPION),
     $            PILEVL(      NIONL,NPION), XLMTHR(      NIONL,NPION),
     $            NPTABL(      NIONL,NPION), CCPLEV(      NIONL,NPION),
     $            SCPLEV(      NIONL,NPION), LLABEL(      NIONL,NPION),
     $            XLMTAB(NDPNT,NIONL,NPION), RCPTAB(NDPNT,NIONL,NPION)
C
C     LIMDAT    = actual number of levels in each model (LIMDAT should
C                 equal LIMPOP in labelled common POPDATA)
C     MAXDATL   = maximum value of LIMDAT
C     LEND      = sum of LIMDAT
C     LLABEL    = "term designation" of each level of each model
C     LLPRNT    = data tables print switch
C     PILEVL    = statistical weight of each level of each model
C     XLMTHR    = threshhold wavelengths of continua
C     NPTABL    = actual number of data points in absorption data table
C                 of each level of each model (used only when > 0)
C     CCPLEV    = threshhold absorption factors
C     SCPLEV    = exponent of power-law wavelength dependence of
C                 absorption of each level of each model (used only
C                 when > 0; should be > 0 when corresponding NPTABL = 0)
C     XLMTAB    = wavelength values for which RCPTAB is specified
C     RCPTAB    = absorption in the continuum of a level of a model
C                 (At wavelengths < XLMTAB(NPTABL), a power law
C                  with exponent = 3 is used.)
C
      common      /IODAT01/ MAXDATL,LEND
      common      /IODAT02/ LIMDAT
      common      /IODAT03/ LLPRNT
      common      /IODAT04/ PILEVL
      common      /IODAT05/ XLMTHR
      common      /IODAT06/ NPTABL
      common      /IODAT07/ CCPLEV
      common      /IODAT08/ SCPLEV
      common      /IODAT09/ LLABEL
      common      /IODAT10/ XLMTAB
      common      /IODAT11/ RCPTAB
C     .
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
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(22),JPOP )
C     !DASH
      external HUN, GOTH, HI, BYE
C
C               TE(N), XNU(NSL), BATAL(N,NL*(NL-1)/2)
      dimension TE(*), XNU(*),   BATAL(*)
C
      call HI ('BUZZARD')
C     !BEG
      if((JPOP.gt.0).and.(LIMPOP(JPOP).gt.NL)) then
        call HUN  (N, NLB, TE, XLMTHR(1,JPOP), BATAL)
      else
        call GOTH (N, NLB, TE, XNU, XNUK, BATAL)
      end if
C     !END
      call BYE ('BUZZARD')
C
      return
      end
