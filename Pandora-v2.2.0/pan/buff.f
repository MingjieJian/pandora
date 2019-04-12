      subroutine BUFF
     $(X,IX,RKI,IQRK,RLI,IQRL,LU,XPBL,ESG,KILROY)
C
C     Rudolf Loeser, 1990 Nov 23
C---- Supervises Upper-Level Charge Exchange calculation.
C     !DASH
      save
C     !DASH
      real*8 ESG, RKI, RLI, X, XPBL
      integer IQCXP, IQRK, IQRL, IX, JJCXP, JJCXX, JJLCX, JJNPQ, JJXRK,
     $        JJXRL, LLPOPK, LLPOPN, LU, LUP, MCXK, N, NL
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(214),JJXRK)
      equivalence (IZOQ(215),JJXRL)
      equivalence (IZOQ(216),JJCXP)
      equivalence (IZOQ(217),JJCXX)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 11),JJLCX)
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
      equivalence (LEST(55),MCXK )
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
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 3),LLPOPK)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(276),IQCXP)
C     !EJECT
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external BURION, ZEUS, BIKOS, POPIO, BULAK, HI, BYE
C
      dimension X(*), IX(*)
C
C               ESG(N,NL), RKI(N,NSL), RLI(N,NSL), IQRL(NSL), IQRK(NSL),
      dimension ESG(*),    RKI(*),     RLI(*),     IQRL(*),   IQRK(*),
C
C               XPBL(Lenpbl)
     $          XPBL(*)
C
      call HI ('BUFF')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call BURION (X, 1, NL, ESG)
      end if
      call ZEUS     (LU, IQCXP, LUP)
      if((MCXK.lt.0).and.(NL.gt.3)) then
C----   Hydrogen
        call BIKOS  (N, NL, RKI, IQRK, RLI, IQRL, LUP, X(JJXRK),
     $               X(JJXRL), ESG)
      else
C----   "XED"-ion
C       Get Hydrogen population data
        call POPIO  ('ASSURE', 1, XPBL)
C----   Charge Exchange effect
        call BULAK  (N, NL, RKI, IQRK, RLI, IQRL, LUP, X(JJCXX),
     $               X(JJCXP), IX(JJNPQ), IX(JJLCX) ,ESG,
     $               XPBL(LLPOPK), XPBL(LLPOPN), LIMPOP(1))
      end if
C     !END
      call BYE ('BUFF')
C
      return
      end
