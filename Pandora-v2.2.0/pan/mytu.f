      subroutine MYTU
     $(LU,XNKPR,XNDPR,BDIPR,BDIJ,BD1,BD0,BDN,BDD,BDR,BDE,BDIUW,BDIW,
     $ BDA,XNE,XNKUW,XNKW,XNK,XNDUW,XNDW,XNDE,XND,BDI)
C
C     Rudolf Loeser, 2003 Jun 24
C---- Produces detailed trace printout for GORSE.
C     (This is version 2 of MYTU.)
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDA, BDD, BDE, BDI, BDIJ, BDIPR, BDIUW, BDIW,
     $       BDN, BDR, WBD, WBDIR, WPOP, XND, XNDE, XNDPR, XNDUW, XNDW,
     $       XNE, XNK, XNKPR, XNKUW, XNKW
      integer IBNVW, IQAMD, IQBDC, IQBED, IQBSM, IQEND, IQVLG, JEDIT,
     $        JNEDP, LU, MBREC, N, NBS, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (IQQ(245),IQBDC)
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(221),IQVLG)
      equivalence (IQQ(161),IQBED)
      equivalence (IQQ(306),IQEND)
      equivalence (IQQ(271),IQBSM)
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
      equivalence (KZQ(197),IBNVW)
      equivalence (RZQ(109),WBDIR)
      equivalence (RZQ( 45),WBD  )
      equivalence (RZQ( 25),WPOP )
      equivalence (KZQ(194),JEDIT)
      equivalence (KZQ(195),MBREC)
      equivalence (KZQ(193),JNEDP)
      equivalence (KZQ(134),NBS  )
C     !DASH
      external ABJECT, LINER, MOTA, MOSO, MASO, MUSY, HI, BYE
C
C               XNDPR(N,NL), BDIPR(N,NL), BDA(N,NL), BD1(N,NL), XNE(N),
      dimension XNDPR(N,*),  BDIPR(N,*),  BDA(N,*),  BD1(N,*),  XNE(*),
C
C               BD0(N,NL), BDN(N,NL), BDD(N,NL), BDIJ(N,NL), BDR(N,NL),
     $          BD0(N,*),  BDN(N,*),  BDD(N,*),  BDIJ(N,*),  BDR(N,*),
C
C               BDE(N,NL), BDIW(N,NL), BDIUW(N,NL), XNDE(N,NL), XNK(N),
     $          BDE(N,*),  BDIW(N,*),  BDIUW(N,*),  XNDE(N,*),  XNK(*),
C
C               XNDUW(N,NL), XND(N,NL), XNDW(N,NL), XNKPR(N), XNKUW(N),
     $          XNDUW(N,*),  XND(N,*),  XNDW(N,*),  XNKPR(*), XNKUW(*),
C
C               BDI(N,NL), XNKW(N)
     $          BDI(N,*),  XNKW(*)
C     !EJECT
C
      call HI ('MYTU')
C     !BEG
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,100) IBNVW
  100   format(' ','Tracing the calculations that led to the ',
     $             'POPULATIONS results printed above.'//
     $         ' ','The steps are illustrated with the values from ',
     $             'depth # IBNVIEW =',I5,'.'/
     $         ' ','   (The default value of IBNVIEW = (JEDIT + 1) ',
     $             '= [(N/2) + 1].)')
        call LINER  (2, LU)
C
C----   Show input
        call MOSO  (LU, IBNVW, N, NL, BDIJ, XNKPR, XNDPR, BDIPR)
C
C----   Show B-development
        call MUSY  (LU, IBNVW, N, NL, IQBDC, IQAMD, IQVLG, IQBED,
     $              IQBSM, NBS, WBDIR, WBD, BD0, BD1, BDN, BDD, BDR,
     $              BDE, BDIUW, BDIW, BDA)
C
C----   Show N-development
        call MASO  (LU, IBNVW, N, NL, WPOP, IQEND, JEDIT, JNEDP, XNE,
     $              XNKUW, XNKW, XNK, XNDUW, XNDW, XNDE, XND)
C
C----   Final B
        call MOTA  (LU, IBNVW, N, NL, MBREC, JNEDP, BDI)
C
        call LINER (1, LU)
        write (LU,101)
  101   format(' ',85X,'(Trace-text was last revised 2005 Aug 25.)')
      end if
C     !END
      call BYE ('MYTU')
C
      return
      end
