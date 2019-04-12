      subroutine KALT
     $(X,W,IW,N,H1,HK,LU)
C
C     Rudolf Loeser, 2004 Dec 09
C---- Computes LTE-values of H1 and HK for lower-level charge exchange.
C
C     ASSUMES   that Hydrogen RABD(i) = 1 everywhere (i.e. no H2).
C     !DASH
      save
C     !DASH
      real*8 H1, HK, W, X, XNUK, dummy
      integer IGM, ILRQ, IN, INPQ, IP, IPF, IS, ISA, ISO, ISUM, IW, IWS,
     $        IXNU, JJHND, JJTE, JJXNE, JN, KLTE, LU, MOX, MUX, N, NL
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(  7),JJTE )
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
C     !DASH
C     !EJECT
      external SALTA, JUJUY, MENDOZA, TUCUMAN, MEDEA, OLWEN, CORDOBA,
     $         WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               HK(N), H1(N)
      dimension HK(*), H1(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IXNU  ),(IN( 2),IP    ),(IN( 3),IPF   ),(IN( 4),ISA   ),
     $(IN( 5),IGM   ),(IN( 6),ISUM  ),(IN( 7),ISO   )
C
      dimension JN(2)
      equivalence
     $(JN( 1),INPQ  ),(JN( 2),ILRQ  )
C
      data KLTE /0/
C
      call HI ('KALT')
C     !BEG
      NL = LIMPOP(1)
C     (Get, and allocate, W & IW allotments)
      call SALTA   (IN, IS,  MOX, 'KALT', NL)
      call JUJUY   (JN, IWS, MUX, 'KALT', NL)
C
C---- Get levels data
      call MENDOZA (NL, IW(INPQ), IW(ILRQ), XNUK, W(IXNU), W(IP))
C---- Get Saha-Boltzmann function and GM
      call TUCUMAN (N, NL, XNUK, W(IXNU), W(IP), X(JJTE), X(JJXNE),
     $              W(IPF), W(ISA), W(IGM))
C
C---- Get HK
      call MEDEA   (   NL, KLTE, X(JJHND), X(JJXNE), W(ISA), dummy,
     $              W(IGM), HK, W(ISUM), W(ISO))
C---- Get H1
      call OLWEN   (1, NL, KLTE, X(JJHND), X(JJXNE), W(ISA), dummy,
     $              W(IGM), H1, W(ISUM), W(ISO))
C
C---- Print
      call CORDOBA (LU, N, NL, IW(INPQ), IW(ILRQ), XNUK, W(IXNU),
     $              W(IP), X(JJTE), X(JJXNE), X(JJHND), W(IPF),
     $              W(ISA), HK, H1, W(IGM))
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'KALT')
      call IGIVE   (IW, 'KALT')
C     !END
      call BYE ('KALT')
C
      return
      end
