      subroutine ARGOSY
     $(XPBL)
C
C     Rudolf Loeser, 1989 Oct 27
C---- Initializes Population Data Blocks processing.
C     XPBL, the block buffer, must be of length .ge. LENPBL.
C     (This is version 3 of ARGOSY.)
C     !DASH
      save
C     !DASH
      real*8 XPBL
      integer IQALP, IQCAP, IQCLP, IQFEP, IQH2P, IQHEP, IQHNP, IQMGP,
     $        IQO2P, IQO3P, IQOXP, IQSIP, IQSOP, IQSUP
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
C     !EJECT
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
      equivalence (IQQ( 36),IQHNP)
      equivalence (IQQ( 27),IQCAP)
      equivalence (IQQ( 74),IQSIP)
      equivalence (IQQ( 48),IQHEP)
      equivalence (IQQ( 90),IQH2P)
      equivalence (IQQ( 91),IQALP)
      equivalence (IQQ( 98),IQMGP)
      equivalence (IQQ(131),IQFEP)
      equivalence (IQQ(209),IQSOP)
      equivalence (IQQ(210),IQCLP)
      equivalence (IQQ(327),IQOXP)
      equivalence (IQQ(125),IQSUP)
      equivalence (IQQ(238),IQO2P)
      equivalence (IQQ(345),IQO3P)
C     !DASH
C     !EJECT
      external ROMP, HI, BYE
C
C               XPBL(Lenpbl)
      dimension XPBL(*)
C
      call HI ('ARGOSY')
C     !BEG
C---- Set up print switches
      IPSWICH( 1) = IQHNP
      IPSWICH( 2) = IQCAP
      IPSWICH( 3) = IQSIP
      IPSWICH( 4) = IQHEP
      IPSWICH( 5) = IQH2P
      IPSWICH( 6) = IQALP
      IPSWICH( 7) = IQMGP
      IPSWICH( 8) = IQFEP
      IPSWICH( 9) = IQSOP
      IPSWICH(10) = IQCLP
      IPSWICH(11) = IQOXP
      IPSWICH(12) = IQSUP
      IPSWICH(13) = IQO2P
      IPSWICH(14) = IQO3P
C
C---- Set up data blocks
      call ROMP (XPBL)
C     !END
      call BYE ('ARGOSY')
C
      return
      end
