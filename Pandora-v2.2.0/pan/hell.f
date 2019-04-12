      subroutine HELL
     $(X,W,KKU,Z,XNU,XLCR,XKX,GKX,IADRS,TNU,OPAC,TK,TNUL,XLB,SP,TRK,
     $ XJIKA,XCBL,IMG)
C
C     Rudolf Loeser, 1975 Sep 26
C---- Deals with Continuum Data for Lyman calculations.
C     !DASH
      save
C     !DASH
      real*8 GKX, OPAC, SP, TK, TNU, TNUL, TRK, W, X, XCBL, XJIKA, XKX,
     $       XLB, XLCR, XNU, XNUK, Z
      integer IADRS, IKKA, IKKB, IMG, IN, IQLYA, IS, KKPR, KKU, KOLEV,
     $        MOX, N, NCR
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(32),NCR)
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
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ( 33),KOLEV)
      equivalence (KZQ( 32),KKPR )
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
      equivalence (IQQ(255),IQLYA)
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
C     !DASH
      external JEAN, TEUFEL, SHAITAN, TERSE, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XCBL(Miklen), TK(N), XNU(NSL), TNUL(N,NCR), TNU(N,KKX),
      dimension XCBL(*),      TK(*), XNU(*),   TNUL(*),     TNU(*),
C
C               XLCR(NCR), OPAC(N,KKX), XLB(N,KKX), XJIKA(N,KKX), Z(N),
     $          XLCR(*),   OPAC(*),     XLB(*),     XJIKA(*),     Z(*),
C
C               IADRS(KKX), TRK(N,KKX), GKX(KKX), XKX(KKX), SP(N,KKX),
     $          IADRS(*),   TRK(*),     GKX(*),   XKX(*),   SP(*),
C
C               IMG(N)
     $          IMG(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IKKA  ),(IN( 2),IKKB  )
C     !EJECT
C
      call HI ('HELL')
C     !BEG
C     (Get, and allocate, W allotment)
      call JEAN       (IN, IS, MOX, 'HELL')
C
C---- Recover and manipulate Continuum Data pertaining to
C     Lyman integration frequencies.
      call TEUFEL     (X, W, XCBL, N, TNU, XLB, SP, TK, KKU, XKX,
     $                 IADRS, XNUK, XNU, NPOPS, OPAC, IMG, W(IKKA),
     $                 W(IKKB), GKX, KOLEV, XJIKA, TRK)
C---- Save Continuum Tau values pertaining to incident
C     coronal radiation
      call SHAITAN    (XCBL, XLCR, NCR, N, TNUL)
      if(IQLYA.le.0) then
C----   (?  Print data for two selected integration frequencies)
        call TERSE    (N, KKU, KKPR, Z, W(IKKA), W(IKKB), TNU, KOLEV)
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'HELL')
C     !END
      call BYE ('HELL')
C
      return
      end
