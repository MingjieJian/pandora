      subroutine ROTHER
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 28
C---- Controls: Lyman rates calculations,
C               number densities calculations,
C               hydrostatic equilibrium calculations or
C               NH-adjustment for constant pressure, and
C               populations updating,
C     which all, in various combinations, fall within the scope
C     of the "HSE-iteration".
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQHSE, IQLYM, IQNES, IW, IX, JPOP, JYDRO, NOION
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
      equivalence (IUPOP( 1),JYDRO)
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
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST(22),JPOP )
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
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ( 16),IQHSE)
      equivalence (IQQ( 56),IQNES)
C     !DASH
C     !EJECT
      external NIDD, COLNE, AIRE, CLYDE, NENE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('ROTHER')
C     !BEG
C---- "Lyman" rates
      if((IQLYM.gt.0).and.(JPOP.gt.0).and.(NOION.le.0)) then
        call NIDD      (X, IX, W, IW)
      end if
      if(IQHSE.gt.0) then
        if(JYDRO.gt.0) then
C----     Number densities
          call COLNE   (1, 0, 0, X, IX, W, IW)
C----     Hydrostatic Equilibrium
          call AIRE    (X, IX, W, IW)
        end if
      else
        if(JPOP.gt.0) then
          if(IQNES.gt.0) then
C----       NE updating
            call CLYDE (X, IX, W, IW)
          end if
        end if
        if(JYDRO.gt.0) then
C----     Adjust NH for constant pressure (if needed),
C         and exhibit gas parameters
          call NENE    (X, IX, W, IW)
        end if
      end if
C---- Number densities
      call COLNE       (1, 1, 1, X, IX, W, IW)
C     !END
      call BYE ('ROTHER')
C
      return
      end
