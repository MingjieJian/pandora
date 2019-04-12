      subroutine RIBEIRO
     $(NO,DOSUM)
C
C     Rudolf Loeser, 1989 Jun 23
C---- Decides whether or not to print an ITERATIVE SUMMARY.
C     (This is version 4 of RIBEIRO.)
C     !DASH
      save
C     !DASH
      integer IQCGR, IQSMT, IQSUM, NO
      logical ANY, DOSUM, NOK, SOM
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
      equivalence (IQQ( 47),IQCGR)
      equivalence (IQQ( 18),IQSUM)
      equivalence (IQQ(284),IQSMT)
C     !EJECT
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
C     !DASH
      external ABJECT, HI, BYE
C
      call HI ('RIBEIRO')
C     !BEG
      SOM = NIBIS.gt.0
      NOK = NIBIS.le.MXIBIS
      ANY = (IQSUM.gt.0).or.(IQCGR.gt.0).or.(IQSMT.gt.0)
C
      DOSUM = SOM.and.NOK.and.ANY
C     !EJECT
      if((NO.gt.0).and.(.not.DOSUM)) then
        if((.not.NOK).and.ANY) then
          call ABJECT (NO)
          write (NO,100) MXIBIS,NIBIS
  100     format(' ','ITERATIVE SUMMARY: capacity exceeded.'//
     $           ' ','Each array of PRD Jnu values, or each vector ',
     $               'of S, RHO, Z, NE, etc., values, constitutes ',
     $               'a "set" for ITERATIVE SUMMARY purposes.'/
     $           ' ','The program is set up to accomodate',I5,
     $               ' sets; this run generated',I8,' sets.'//
     $           ' ','All the "excess" sets were ignored; ',
     $               'and no attempt is now made to print ',
     $               'meaningful SUMMARYs.'//
     $           ' ','Try setting up a run with fewer iterations, ',
     $               'or try reducing the number of sets by ',
     $               'turning off some of the "ITER..." options.')
        end if
        if(.not.ANY) then
          call ABJECT (NO)
          write (NO,101)
  101     format(' ','No iterative summaries appear because none of ',
     $               'the options SUMMARY, CHKGRAF, or SUMTREND ',
     $               'is on.')
        end if
        if((.not.SOM).and.ANY) then
          call ABJECT (NO)
          write (NO,102)
  102     format(' ','No iterative summaries are printed because ',
     $               'no summary data sets were collected'/
     $           ' ','(check the detailed "ITER..." options for the ',
     $               'various summaries).')
        end if
      end if
C     !END
      call BYE ('RIBEIRO')
C
      return
      end
