      subroutine POPIO
     $(OPER,NPOP,BUFFER)
C
C     Rudolf Loeser, 1982 Jun 01
C
C---- Moves population data between the "BUFFER" in memory
C     and records in a random-access disk file.
C
C     Note: "NPOP" is the 'population ion identifier' for the
C           data of the current record, and also serves as the
C           record name. Thus it is a location in "BUFFER".
C           (It is actually in floating point form when part of
C           the record, but is generally converted to integer
C           form and used in integer form in the code.)
C     !DASH
      save
C     !DASH
      real*8 BUFFER
      integer IPOP, JAGNE, JALCI, JARBO, JELI2, JELIU, JILIC, JIRON,
     $        JLUMI, JODIU, JYDRO, LLBD, LLIUP, LLNPOP, LLPOPK, LLPOPN,
     $        NAL, NCA, NFE, NLC, NLH, NLS, NLZ, NMG, NNA, NPOP, NZ2
      character OPER*(*)
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
C     !DASH
C     !EJECT
C     "OPER" can be one of five commands:
C
C     "INIT"     : Initialize "BUFFER" (i.e. set BUFFER(LLNPOP)=0.)
C                  When "OPER" = "INIT", then the argument "NPOP"
C                  is not used.
C
C     "WRITE"    : If "BUFFER" currently contains valid data, then
C                  write it to the appropriate record on disk
C                  (it will be rewritten if the corresponding
C                  record address is .gt. 0, otherwise it will be
C                  written and the new record address will be saved);
C                  otherwise, do nothing.
C                  When "OPER" = "WRITE", then the argument "NPOP"
C                  is not used.
C
C     "READ"     : If "NPOP" is a valid population ion identifier,
C                  then read the corresponding record into "BUFFER"
C                  and verify that "BUFFER" now contains the
C                  desired data (if not, then stop: serious error!);
C                  otherwise, do nothing.
C
C     "ASSURE"   : If "NPOP" is a valid population ion identifier,
C                  and if either "BUFFER" does not contain valid data,
C                  or "BUFFER" currently contains valid data and
C                  the population ion identifier of the data in
C                  "BUFFER" is not the same as "NPOP", then do "READ";
C                  otherwise, do nothing.
C
C     "EXCHANGE" : If "NPOP" is a valid population ion identifier,
C                  and if the population ion identifier contained in
C                  "BUFFER" is not the same as "NPOP", then, if the
C                  latter is valid, first do "WRITE", then do "READ";
C                  otherwise, do nothing.
C
C     NOTE: The current contents of "BUFFER" are presumed to be valid
C     data if the population ion identifier contained in "BUFFER"
C     (i.e. BUFFER(LLNPOP)) is a valid population ion identifier.
C     A valid population ion identifier is an integer whose value
C     is .ge. 1 and .le. NPI; for NPI, see labelled common "POPDATA".
C     !DASH
      external POPCORN, HI, BYE
C
C               BUFFER(Lenpbl)
      dimension BUFFER(*)
C
      call HI ('POPIO')
C     !BEG
      if(OPER.eq.'INIT') then
        IPOP = 0
        BUFFER(LLNPOP) = IPOP
      else
        call POPCORN (OPER, NPOP, BUFFER)
      end if
C     !END
      call BYE ('POPIO')
C
      return
      end
