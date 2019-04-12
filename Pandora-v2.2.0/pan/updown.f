      subroutine UPDOWN
     $(XNK,XND,BDI, POPK,POP,BD, LIMP,LIM)
C
C     Rudolf Loeser, 1988 May 04
C---- Puts number density and departure coefficient data
C     of the "ion of the run" into "population ion data" slots.
C     (This is version 2 of UPDOWN.)
C     !DASH
      save
C     !DASH
      real*8 BD, BDI, ONE, POP, POPK, XND, XNK
      integer JP, JR, LIM, LIMP, N, NL
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  MOVE1, ZERO1, SET1, HI, BYE
      intrinsic min
C
C               XND(N,NL), BDI(N,NL), POP(N,LIMP), BD(N,LIMP), POPK(N),
      dimension XND(N,*),  BDI(N,*),  POP(N,*),    BD(N,*),    POPK(*),
C
C               XNK(N)
     $          XNK(*)
C
      call HI ('UPDOWN')
C     !BEG
      call MOVE1     (XNK, N, POPK)
C
      LIM = min(LIMP,MRTPA)
C
      do 100 JP = 1,LIM
        JR = MRTP(JP)
        call MOVE1   (XND(1,JR), N, POP(1,JP))
        call MOVE1   (BDI(1,JR), N, BD(1,JP) )
  100 continue
C
      if(LIMP.gt.LIM) then
        do 101 JP = (LIM+1),LIMP
          call ZERO1 (POP(1,JP), N)
          call SET1  (BD(1,JP),  N, (-ONE))
  101   continue
      end if
C     !END
      call BYE ('UPDOWN')
C
      return
      end
