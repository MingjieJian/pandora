      subroutine MARCEL
C
C     Rudolf Loeser, 2002 Jul 11
C---- Massages ion-data counters, for FORUM.
C     (See also SAMPO.)
C     !DASH
      save
C     !DASH
      integer IMAX, jummy
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
C     .
C     NPOPS   = total number of population data blocks = NPI;
C     MAXPOPL = maximum value of LIMPOP;
C     LENT    = sum of LIMPOP;
C     LENPBL  = length of a Population Data Block;
C     LZOQ    = Population Data Blocks components index, (POPN and BD
C               are arrays of the form [N,LIMP]);
C     LENPOP  = number of non-LTE population levels in a set;
C     LIMPOP  = total number of population levels in a set;
C     NAMES   = population ion names;
C     NAMKNT  = number of characters in NAMES;
C     TNAMES  = truncated population ion names;
C     IUPOP   = populations update switches;
C     IBLAD   = file address of Population Block records;
C     IPSWICH = copy of population data print option setting;
C     POPMSS  = ion mass;
C     POPSYM  = element symbol of population ion, as in ELEMENT table;
C     KAPNO   = absorber (Kappa) number of population ion;
C     ICKSM   = index of "SENNA" checksum;
C     MRTP    = "ion-of-run"-to-"population-ion-model" level indices
C               (length must be .ge. MAXPOPL). [MRTP(I)=J means:
C               level J of the "ion-of-run" correseponds to level I of
C               the built-in "population-ion-model"];
C     KLABPI, NLABPI, BLABPI = names of population in data tables, as
C                              used in input statements.
C     .
      equivalence
     $(LZOQ( 1),LLNPOP),(LZOQ( 2),LLIUP ),(LZOQ( 3),LLPOPK),
     $(LZOQ( 4),LLPOPN),(LZOQ( 5),LLBD  )
      equivalence
     $(LENPOP( 1),NLH ),(LENPOP( 2),NLC ),(LENPOP( 3),NLS ),
     $(LENPOP( 4),NLZ ),(LENPOP( 5),NZ2 ),(LENPOP( 6),NAL ),
     $(LENPOP( 7),NMG ),(LENPOP( 8),NFE ),(LENPOP( 9),NNA ),
     $(LENPOP(10),NCA ),(LENPOP(11),NLO ),(LENPOP(12),NLU ),
     $(LENPOP(13),NO2 ),(LENPOP(14),NO3 )
      equivalence
     $(IUPOP( 1),JYDRO),(IUPOP( 2),JARBO),(IUPOP( 3),JILIC),
     $(IUPOP( 4),JELIU),(IUPOP( 5),JELI2),(IUPOP( 6),JLUMI),
     $(IUPOP( 7),JAGNE),(IUPOP( 8),JIRON),(IUPOP( 9),JODIU),
     $(IUPOP(10),JALCI),(IUPOP(11),JOXYG),(IUPOP(12),JULPH),
     $(IUPOP(13),JOXY2),(IUPOP(14),JOXY3)
C     .
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
C     !DASH
      external MINMAXI, IRRSUM, HI, BYE
C
      call HI ('MARCEL')
C     !BEG
      call IRRSUM  (LIMDAT, NPOPS, LEND)
      call MINMAXI (LIMDAT, 1, NPOPS, jummy, IMAX)
      MAXDATL = LIMDAT(IMAX)
C     !END
      call BYE ('MARCEL')
C
      return
      end
