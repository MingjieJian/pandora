      subroutine WARLOCK
     $(LUPR,LUMR,HND,BDHM,XNE,ZME,XNC,GMASS,TE,PGS,PTO,VT,VM,GD,T5,
     $ VBMB,XPBL)
C
C     Rudolf Loeser, 1980 Jan 04
C---- Puts "atmospheric" data into restart files.
C     !DASH
      save
C     !DASH
      real*8 BDHM, GD, GMASS, HND, PGS, PTO, T5, TE, VBMB, VM, VT, XNC,
     $       XNE, XPBL, ZME
      integer IQHMS, IQHSE, JPOP, JYDRO, KAMB, KPRSW, KVLG, LUMR, LUPR,
     $        N
      logical ZVB, ZZME
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
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
      equivalence (IQQ( 16),IQHSE)
      equivalence (IQQ( 68),IQHMS)
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
      equivalence (LEST( 6),KPRSW)
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
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
      equivalence (IUPOP( 1),JYDRO)
C     !DASH
C     !EJECT
      external  NAUGHTD, BUNT, AHOY, HI, BYE
      intrinsic max
C
C               XPBL(Lenpbl), HND(N), BDHM(N), XNE(N), GMASS(N), T5(N),
      dimension XPBL(*),      HND(*), BDHM(*), XNE(*), GMASS(*), T5(*),
C
C               TE(N), PGS(N), PTO(N), VT(N), VBMB(N), XNC(N), ZME(N),
     $          TE(*), PGS(*), PTO(*), VT(*), VBMB(*), XNC(*), ZME(*),
C
C               VM(N), GD(N)
     $          VM(*), GD(*)
C
      call HI ('WARLOCK')
C     !BEG
      call NAUGHTD    (ZME,  1, N, ZZME)
      call NAUGHTD    (VBMB, 1, N, ZVB )
C
  100 format(A80)
C
      if((JPOP.gt.0).or.(IQHSE.gt.0)) then
        if(JYDRO.gt.0) then
          write (LUPR,100) HEAD
          call BUNT   (LUPR, XNE,   'NE')
          if(.not.ZZME) then
            write (LUPR,100) HEAD
            call BUNT (LUPR, ZME,   'ZME')
          end if
          write (LUPR,100) HEAD
          call BUNT   (LUPR, XNC,   'NC')
        else
          write (LUMR,100) HEAD
          call BUNT   (LUMR, XNE,   'NE')
          write (LUMR,100) HEAD
          call BUNT   (LUMR, ZME,   'ZME')
          call BUNT   (LUMR, XNC,   'NC')
        end if
      end if
C     !EJECT
      if(IQHMS.gt.0) then
        write (LUPR,100) HEAD
        call BUNT     (LUPR, BDHM,  'BDHM')
      end if
      if((max(KAMB,KVLG).eq.1).and.(.not.ZVB)) then
        write (LUPR,100) HEAD
        call BUNT     (LUPR, VBMB,  'VBMB')
      end if
      if(IQHSE.gt.0) then
        write (LUPR,100) HEAD
        call BUNT     (LUPR, HND,   'NH')
        write (LUMR,100) HEAD
        call BUNT     (LUMR, GMASS, 'MASS')
        call BUNT     (LUMR, PGS,   'PGS')
        call BUNT     (LUMR, PTO,   'PTO')
        call BUNT     (LUMR, TE,    'TE')
        call BUNT     (LUMR, VT,    'VT')
        call BUNT     (LUMR, VM,    'VM')
        call BUNT     (LUMR, GD,    'DENS')
        call BUNT     (LUMR, T5,    'TAU5000')
      else if(KPRSW.gt.0) then
        write (LUPR,100) HEAD
        call BUNT     (LUPR, HND,   'NH')
      end if
      call AHOY       (LUPR, XPBL)
C     !END
      call BYE ('WARLOCK')
C
      return
      end
