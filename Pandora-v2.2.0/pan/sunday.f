      subroutine SUNDAY
     $(X,W,LUE,BDI,XNE,HND,ZHEL,ZME,ZRN,XPBL,YPBL)
C
C     Rudolf Loeser, 1981 Jun 22
C     Revised RL/SGK Apr  9 2014 
C---- Updates NE.
C     !DASH
      save
C     !DASH
      real*8 BDI, HND, W, X, XNE, XPBL, YPBL, ZHEL,ZME,ZRN
      integer IQNES, LLPOPK, LLPOPN, LUE, N, NLH
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LENPOP( 1),NLH )
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
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
      equivalence (IQQ( 56),IQNES)
C     !DASH
      external POPIO, MOVE1, TUESDAY, FRAGA, ANGRY, WENDY, HI, BYE
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl), YPBL(Lenpbl), XNE(N), ZHEL(N), BDI(N,NL),
      dimension XPBL(*),      YPBL(*),      XNE(*), ZHEL(*), BDI(*),
C
C               HND(N), ZME(N), ZRN(N)
     $          HND(*), ZME(*), ZRN(*)
C
      call HI ('SUNDAY')
C     !BEG
C---- Read Hydrogen populations data
      call POPIO     ('ASSURE', 1, XPBL)
C---- Compute
      if(IQNES.le.0) then
C----   Set XNE = XNP (and set up ZRN and XNC)
        call MOVE1   (XPBL(LLPOPK), N, XNE)
        call ANGRY   (X, ZME, ZHEL, ZRN)
        call FRAGA   (X, YPBL)
      else
C----   Compute XNE from the quadratic expression
        call TUESDAY (X, W, YPBL, LUE, BDI, XNE, HND, ZHEL,
     $                XPBL(LLPOPN), NLH)
      end if
C---- Continuum Recalculation control
      call WENDY     (XNE, 1, N, 3, 'SUNDAY')
C     !END
      call BYE ('SUNDAY')
C
      return
      end
