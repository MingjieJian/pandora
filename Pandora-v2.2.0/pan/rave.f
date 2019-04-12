      subroutine RAVE
     $(NO)
C
C     Rudolf Loeser, 2003 Apr 22
C---- Prints header and explanation for iterative summaries.
C     !DASH
      save
C     !DASH
      real*8 WPRSS, ZERO
      integer IQDT2, IQHSE, IQIBD, IQICK, IQIDP, IQINE, IQINH, IQINN,
     $        IQIRH, IQIRK, IQIRW, IQISS, IQITA, IQITD, IQIZZ, IQLYM,
     $        IQND2, IQNES, IQQHR, IQSMG, IQSMT, JPOP, KMASN, KPRSW,
     $        KTKIN, NDT, NO, jummy
      character QNAME*8, TEXT*3
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(21),NDT)
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
      equivalence (RZQ( 87),WPRSS)
      equivalence (QZQ(  1),QNAME)
C
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
      equivalence (LEST(11),KTKIN)
      equivalence (LEST(30),KMASN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ( 99),IQDT2)
      equivalence (IQQ(212),IQND2)
      equivalence (IQQ( 16),IQHSE)
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ(283),IQSMG)
      equivalence (IQQ(128),IQIBD)
      equivalence (IQQ(113),IQINN)
      equivalence (IQQ(127),IQINE)
      equivalence (IQQ(201),IQINH)
      equivalence (IQQ(110),IQIRH)
      equivalence (IQQ(112),IQIRK)
      equivalence (IQQ(114),IQIRW)
      equivalence (IQQ(109),IQISS)
      equivalence (IQQ(111),IQITA)
      equivalence (IQQ(213),IQITD)
      equivalence (IQQ(136),IQIZZ)
      equivalence (IQQ(302),IQICK)
      equivalence (IQQ(284),IQSMT)
      equivalence (IQQ(142),IQIDP)
      equivalence (IQQ(323),IQQHR)
C     !DASH
      external PRIAM, ONOFF, LINER, HI, BYE
C
      call HI ('RAVE')
C     !BEG
      if(NO.le.0) then
        goto 199
      end if
C     !EJECT
      call PRIAM   (NO, 'ITERATES', 8)
C
      call LINER   (1, NO)
      write (NO,100)
  100 format(' ','(The ITERATES printout appears because option ',
     $           'SUMMARY=on or option SUMTREND=on.)'///
     $       ' ','Printing of iterative summaries is controlled in ',
     $           'detail by the options in the following table'/
     $       ' ','(which also lists the status of each option in ',
     $           'this run):')
  101 format(' ',A8,2X,A3,3X,A)
C
      call LINER   (1, NO)
      call ONOFF   (IQISS, jummy, TEXT)
      write (NO,101) 'ITERS   ',TEXT,'S, line source functions'
      call ONOFF   (IQIRH, jummy, TEXT)
      write (NO,101) 'ITERRHO ',TEXT,'RHO, net radiative bracket'
      call ONOFF   (IQIRW, jummy, TEXT)
      write (NO,101) 'ITERRWT ',TEXT,'RHO weights'
      call ONOFF   (IQQHR, jummy, TEXT)
      write (NO,101) 'ITERCHI ',TEXT,'CHI, a RHO-like quantity'
      call ONOFF   (IQITA, jummy, TEXT)
      write (NO,101) 'ITERTAU ',TEXT,'TAU, line-center optical depth'
      call ONOFF   (IQICK, jummy, TEXT)
      write (NO,101) 'ITERCHK ',TEXT,'CHECKs (b-ratios consistency)'
      call ONOFF   (IQINN, jummy, TEXT)
      write (NO,101) 'ITERN   ',TEXT,'number densities'
      if(IQLYM.gt.0) then
        call ONOFF (IQIBD, jummy, TEXT)
        write (NO,101) 'ITERB   ',TEXT,'b(KOLEV), departure coefficient'
        call ONOFF (IQIRK, jummy, TEXT)
        write (NO,101) 'ITERRK  ',TEXT,'RK weights'
      end if
      if((IQHSE.gt.0).and.(JPOP.gt.0).and.(IQNES.gt.0)) then
        call ONOFF (IQINE, jummy, TEXT)
        write (NO,101) 'ITERNE  ',TEXT,'NE, electron density'
      end if
      if((IQHSE.gt.0).or.((KPRSW.gt.0).and.(WPRSS.ne.ZERO))) then
        call ONOFF (IQINH, jummy, TEXT)
        write (NO,101) 'ITERNH  ',TEXT,'NH, total hydrogen density'
      end if
      if((IQDT2.gt.0).and.(IQND2.gt.0).and.(NDT.gt.0)) then
        call ONOFF (IQITD, jummy, TEXT)
        write (NO,101) 'ITERTD  ',TEXT,'dust temperature'
      end if
      if((KMASN.gt.0).or.((KTKIN.gt.0).and.(QNAME.eq.'HYDROGEN'))) then
        call ONOFF (IQIZZ, jummy, TEXT)
        write (NO,101) 'ITERZ   ',TEXT,'Z, geometrical depth grid'
      end if
C     !EJECT
      call LINER (1,NO)
      call ONOFF (IQSMG, jummy, TEXT)
      write (NO,101) 'SUMGRAF ',TEXT,'iterative summaries as graphs'
      write (NO,102)
  102 format(' ',16X,'The format of iterative summaries is controlled ',
     $           'by the switch ISMSW and the option SUMGRAF.'/
     $       ' ',16X,'A summary can be either a numerical table or ',
     $           'a compact graph.'/
     $       ' ',16X,'When ISMSW = 1, both will be provided; however'/
     $       ' ',16X,'when ISMSW = 0, then the graph is printed when ',
     $           'SUMGRAF=on, but the table when SUMGRAF=off.')
C
      call LINER (1,NO)
      call ONOFF (IQSMT, jummy, TEXT)
      write (NO,101) 'SUMTREND',TEXT,'iterative trend summary'
      write (NO,103)
  103 format(' ',16X,'Note: option SUMTREND provides a compact ',
     $           '(1-line) summary of the last 3 iterations.')
C
      call LINER (1,NO)
      call ONOFF (IQIDP, jummy, TEXT)
      write (NO,101) 'ITDMP   ',TEXT,'dump of summary set statistics'
      write (NO,104)
  104 format(' ',16X,'Note: option ITDMP is mainly useful for program ',
     $           'checkout.')
C
      call LINER (2,NO)
      write (NO,105)
  105 format(' ','Iterative Summaries can appear ONLY if there is ',
     $           'more than one set of results; (note that some ',
     $           'quantities'/
     $       ' ',20X,'are computed more than once per "iteration").')
C
  199 continue
C     !END
      call BYE ('RAVE')
C
      return
      end
