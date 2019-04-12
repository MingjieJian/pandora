      subroutine REED
     $(X,W,IW,XLM,DUMP,JNUMTH,TAU,BHS,SCAT,CNDT,CNXP,OPAC,SIGMA,BHSNUM,
     $ II,NR,N,YDAMP,XJNU,SOURCE,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes XJNU and SOURCE, by one of two different methods,
C     for LILITH.
C     (See also "CANE".)
C     (This is version 3 of REED.)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNDT, CNXP, CSFCT, OPAC, SCAT, SIGMA, SOURCE,
     $       TAU, W, X, XJNU, XLM, YDAMP
      integer II, IMG, IQINC, ITS, IW, JJZ, JNUMTH, LAG, N, NR
      logical DUMP
C     !COM
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
C     !DASH
C     !EJECT
      external TUAREG, PANKU, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               SIGMA(N), TAU(N), BHSNUM(N), SCAT(N), CNDT(N), CNXP(N),
      dimension SIGMA(*), TAU(*), BHSNUM(*), SCAT(*), CNDT(*), CNXP(*),
C
C               OPAC(N), BHS(N), XJNU(N), SOURCE(N), IMG(N)
     $          OPAC(*), BHS(*), XJNU(*), SOURCE(*), IMG(*)
C
      call HI ('REED')
C     !BEG
      if(JNUMTH.eq.1) then
        call TUAREG (X, W, IW, XLM, DUMP, TAU, BHS, BHSNUM, SIGMA,
     $               CNXP, X(JJZ), OPAC, II, NR, N, YDAMP, IQINC,
     $               CSFCT, XJNU, SOURCE, ITS, LAG, IMG)
      else
        call PANKU  (X, W, IW, XLM, DUMP, TAU, BHS, SCAT, CNDT,
     $               CNXP, X(JJZ), OPAC, II, NR, N, YDAMP, IQINC,
     $               CSFCT, XJNU, SOURCE, ITS, LAG, IMG)
      end if
C     !END
      call BYE ('REED')
C
      return
      end
