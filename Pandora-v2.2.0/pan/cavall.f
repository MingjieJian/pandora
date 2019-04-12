      subroutine CAVALL
     $(X,W,BH,BHE1,BHE2,S,EV,NO)
C
C     Rudolf Loeser, 1984 May 18
C---- Drives particle energy dissipation calculation.
C     NO is the unit number for regular printout from the fast-electrons
C     calculation.
C     !DASH
      save
C     !DASH
      real*8 BH, BHE1, BHE2, ELLED, EMXED, EV, S, W, X
      integer IQPED, JJTE, JJXNE, JJZ, LLPOPK, LLPOPN, N, NO, NSPED
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
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
      equivalence (RZQ( 78),ELLED)
      equivalence (RZQ( 79),EMXED)
      equivalence (KZQ( 77),NSPED)
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
      equivalence (IQQ(175),IQPED)
C     !EJECT
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
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
C     !DASH
      external ZERO1, POPIO, PENDANT, HI, BYE
C
      dimension X(*), W(*)
C
C               BH(Lenpbl), BHE1(Lenpbl), BHE2(Lenpbl), EV(N), S(N)
      dimension BH(*),      BHE1(*),      BHE2(*),      EV(*), S(*)
C     !EJECT
C
      call HI ('CAVALL')
C     !BEG
      call ZERO1     (EV, N)
      call ZERO1     (S , N)
      if(IQPED.gt.0) then
        call POPIO   ('ASSURE', 1, BH  )
        call POPIO   ('ASSURE', 4, BHE1)
        call POPIO   ('ASSURE', 5, BHE2)
        call PENDANT (N, X(JJZ), X(JJTE), X(JJXNE), BH(LLPOPK),
     $                BH(LLPOPN), BHE1(LLPOPK), BHE1(LLPOPN),
     $                BHE2(LLPOPK), BHE2(LLPOPN), NSPED, ELLED, EMXED,
     $                NO, S, EV, W)
      end if
C     !END
      call BYE ('CAVALL')
C
      return
      end
