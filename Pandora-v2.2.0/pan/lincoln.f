      subroutine LINCOLN
     $(X,W,IW,XLB1,XLB2,XLM,DUMP,TAU,BHS,SCAT,CNDT,CNXP,OPAC,SIGMA,
     $ BHSNUM,N,YDAMP,MOVING,ILFLX,JNUMTH,XJNU,SOURCE,ITS,LAG,XLTIT,
     $ IMG)
C
C     Rudolf Loeser, 1983 Jan 21
C---- Computes XJNU and SOURCE, by one of two different methods.
C
C     (Special version of "REED", for P.R.D. Jnu calculation.)
C                                     ==========
C
C     Also, save matrices for line source function computation.
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNDT, CNXP, CSFCT, OPAC, SCAT, SIGMA, SOURCE,
     $       TAU, W, X, XJNU, XLB1, XLB2, XLM, XLTIT, YDAMP
      integer ILFLX, IMG, IN, IPBLK, IQINC, IS, ITS, IW, IWH, IWN, JJZ,
     $        JNUMTH, LAG, MOX, N
      logical DUMP, MOVING
C     !COM
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
      equivalence (IQQ( 51),IQINC)
C     !EJECT
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (RZQ( 69),CSFCT)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external RAPIER, ELKIN, BERBER, IBORA, HALT, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CNXP(N), TAU(N), BHS(N), SCAT(N), BHSNUM(N), SOURCE(N),
      dimension CNXP(*), TAU(*), BHS(*), SCAT(*), BHSNUM(*), SOURCE(*),
C
C               XLB1(Li1len), XLB2(Li2len), SIGMA(N), OPAC(N), XJNU(N),
     $          XLB1(*),      XLB2(*),      SIGMA(*), OPAC(*), XJNU(*),
C
C               IMG(N), CNDT(N)
     $          IMG(*), CNDT(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IWN   ),(IN( 2),IPBLK ),(IN( 3),IWH   )
C
      call HI ('LINCOLN')
C     !BEG
C     (Get, and allocate, W allotment)
      call RAPIER   (IN, IS, MOX, 'LINCOLN')
C
      if(JNUMTH.eq.1) then
        call BERBER (X, W, IW, XLM, DUMP, TAU, BHS, BHSNUM, SIGMA,
     $               CNXP, X(JJZ), OPAC, N, YDAMP, IQINC, CSFCT, XJNU,
     $               SOURCE, W(IWN), W(IWH), MOVING, ILFLX, ITS, LAG,
     $               IMG)
      else if(JNUMTH.eq.0) then
        call IBORA  (X, W, IW, XLM, DUMP, TAU, BHS, SCAT  , CNDT ,
     $               CNXP, X(JJZ), OPAC, N, YDAMP, IQINC, CSFCT, XJNU,
     $               SOURCE, W(IWN), W(IWH), MOVING, ILFLX, ITS, LAG,
     $               IMG)
      else
        write (MSSLIN(1),100) JNUMTH
  100   format('JNUMTH =',I12,', which is not 0 or 1.')
        call HALT   ('LINCOLN', 1)
      end if
C
      call ELKIN    (W(IWN), W(IWH), ILFLX, W(IPBLK), XLTIT)
C
C     (Give back W allotment)
      call WGIVE    (W, 'LINCOLN')
C     !END
      call BYE ('LINCOLN')
C
      return
      end
