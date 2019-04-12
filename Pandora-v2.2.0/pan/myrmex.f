      subroutine MYRMEX
     $(NPOP,KODE)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Checks whether Population Data "NPOP" is needed.
C     Returns with KODE = 1 if yes, = 0 if no.
C     If no, also sets LENPOP(NPOP) = 0.
C     (This is version 2 of MYRMEX.)
C     !DASH
      save
C     !DASH
      real*8 AB, ZERO, dummy
      integer I, J, KODE, KOP, LODE, LSTH, NHL, NPOP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external FRANK, HI, BYE
C
      parameter (NHL=10)
      dimension LSTH(NHL)
C
C     KAPNOs of absorbers needing Hydrogen populations
      data LSTH /1, 2, 4, 9, 11, 16, 28, 34, 36, 37/
C
      call HI ('MYRMEX')
C     !BEG
      KODE = 1
      KOP  = KAPNO(NPOP)
      if(KOPAC(KOP).le.0) then
        KODE = 0
      end if
C
      if(NPOP.eq.4) then
        if(KOPAC(21).gt.0) then
          KODE = 1
        end if
      else if(NPOP.eq.5) then
        if(KOPAC(20).gt.0) then
          KODE = 1
        end if
      else if(NPOP.eq.1) then
        do 100 I = 1,NHL
          J = LSTH(I)
          if(KOPAC(J).gt.0) then
            KODE = 1
          end if
  100   continue
      end if
C
      if(NPOP.ne.1) then
        call FRANK (POPSYM(NPOP), 0, AB, dummy, dummy, dummy, LODE)
        if((LODE.eq.0).or.(AB.eq.ZERO)) then
          KODE = 0
        end if
      end if
      if(KODE.eq.0) then
        LENPOP(NPOP) = 0
      end if
C     !END
      call BYE ('MYRMEX')
C
      return
      end
