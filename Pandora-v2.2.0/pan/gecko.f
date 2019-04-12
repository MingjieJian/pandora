      subroutine GECKO
     $(X,IX,XLB1,CRD,CVW,CSK,CRS,YLI,SEM,OLN,PRD,DPC,SFT,GMA,YCO,OML,
     $ MTR,JU,JL,KTR,KU,KL,KDDR,KDRX,KNZGM,WAV,UIR,KIJ,FLX,DLL,SFP,
     $ FDB,OLL,SBI,WVN,CKH,BOC,LIJ,DPM,PXC,PXP,PXR,PCE)
C
C     Rudolf Loeser, 1980 Dec 27
C---- Sets up transition data for printing.
C     !DASH
      save
C     !DASH
      real*8 BOC, CKH, CRD, CRS, CSK, CVW, DLL, DPC, DPM, FDB, FLX, GMA,
     $       OLL, OLN, OML, PCE, PRD, PXC, PXP, PXR, SBI, SEM, SFP, SFT,
     $       UIR, WAV, WVN, X, XLB1, YCO, YLI
      integer IQCEF, IQPMH, IX, JJKIJ, JJLIJ, JJOLL, JJOML, JJPCE,
     $        JJYCO, JL, JU, KDDR, KDRX, KIJ, KL, KNZGM, KTR, KU, LIJ,
     $        MTR, NL, NT
      logical CFUSE, DRUSE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ(  3),JJOML)
      equivalence (IZOQ(182),JJOLL)
      equivalence (IZOQ(249),JJPCE)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ(  8),JJLIJ)
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
      equivalence (IQQ(337),IQPMH)
      equivalence (IQQ(328),IQCEF)
C     !DASH
      external BUFFALO, OTTER, JACKAL, CRACK, JEKYLL, LAMIT, ZORILLE,
     $         HI, BYE
C
      dimension X(*), IX(*)
C
C               CRD(MUL), CVW(MUL), CSK(MUL), SBI(MUL), OLL(MUL),
      dimension CRD(*),   CVW(*),   CSK(*),   SBI(*),   OLL(*),
C
C               CRS(MUL), YLI(MUL), SEM(MUL), OLN(MUL), PRD(MUL),
     $          CRS(*),   YLI(*),   SEM(*),   OLN(*),   PRD(*),
C
C               SFT(MUL), GMA(MUL), PCE(MUL), YCO(MUL), OML(MUL),
     $          SFT(*),   GMA(*),   PCE(*),   YCO(*),   OML(*),
C
C               JL (MUL), WAV(MUL), UIR(MUL), KIJ(MUL), FLX(MUL),
     $          JL (*),   WAV(*),   UIR(*),   KIJ(*),   FLX(*),
C
C               FDB(MUL), WVN(MUL), KU(MUL) , KL(MUL) , CKH(MUL),
     $          FDB(*),   WVN(*),   KU(*) ,   KL(*) ,   CKH(*),
C
C               LIJ(MUL), BOC(MUL), SFP(MUL), DPC(MUL), DLL(MUL),
     $          LIJ(*),   BOC(*),   SFP(*),   DPC(*),   DLL(*),
C
C               JU (MUL), DPM(MUL), PXC(MUL), PXP(MUL), PXR(MUL),
     $          JU (*),   DPM(*),   PXC(*),   PXP(*),   PXR(*),
C
C               XLB1(Li1len)
     $          XLB1(*)
C     !EJECT
C
      call HI ('GECKO')
C     !BEG
      DRUSE = IQPMH.le.0
      CFUSE = IQCEF.gt.0
C---- Set up all possible transition index pairs [JU/JL], and their
C     count, MTR. (Note: MTR should equal NL*(NL-1)/2.) (This sets up
C     the "first part" of the LIZARD printout)
      call JACKAL  (NL, JU, JL, MTR)
C     The values JU(i) and JL(i) now are the index pairs for the
C     i'th slot in all the data arrays; to access a datum for any
C     particular pair (JU,JL), INDXUL indexing must be used.
C
C---- Get KIJ and LIJ for all transition index pairs
      call LAMIT   (JU, JL, MTR, IX(JJKIJ), IX(JJLIJ), NL, KIJ, LIJ)
C
C---- Get data from Line Intensity data blocks and from LIMBO
      call BUFFALO (XLB1, NT, JU, JL, CRD, CVW, CSK, CRS, YLI, SEM,
     $              OLN, PRD, DPC, SFT, GMA, KDDR, KDRX, KNZGM, UIR,
     $              FLX, DLL, SFP, FDB, SBI, WAV, WVN, CKH, BOC, DPM,
     $              PXC, PXP, PXR)
C
C---- Get other data from General (in-memory) data block
      call OTTER   (JU, JL, X, NT, X(JJYCO), X(JJOML), X(JJOLL),
     $              X(JJPCE), YCO, OML, OLL, PCE)
C
C---- Massage and apply biases to switch settings.
      call CRACK   (JU, JL, MTR, KIJ, SEM, SFT, YLI, YCO, DPC, OLN,
     $              PRD, GMA, UIR, FLX, SFP, FDB, WAV, BOC, PXC, PXP,
     $              PXR, DRUSE, PCE, CFUSE)
C
C---- Do not print anything but AIJ for THICK transitions
      call ZORILLE (MTR, KIJ, CRD, CVW, CSK, CRS, DPC, PRD, GMA, DPM,
     $              PXC, PXP, PXR)
C
C---- Set up radiative transition index pairs [KU/KL], and their
C     count, KTR (to set up "second part" of the LIZARD printout)
      call JEKYLL  (JU, JL, MTR, KIJ, KU, KL, KTR)
C     !END
      call BYE ('GECKO')
C
      return
      end
