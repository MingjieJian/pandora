      subroutine BUPPI
     $(X,XLM,KTRU,PRNT)
C
C     Rudolf Loeser, 2002 Aug 16
C---- Determines whether to print anything for this wavelength.
C     (KTRU = 1 for "True Continuum" wavelengths.)
C     (This is version 2 of BUPPI.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, X, XLM
      integer I, IQAPC, IQCCP, IQCDP, IQCPA, IQCPC, IQCPL, IQDTP, IQHMP,
     $        IQKSP, IQLYC, IQOST, IQPFC, IQSBP, IQTCP, J27, J5, J6,
     $        JFDB, JJSCW, KTRU, NSW
      logical PRNT
C     !COM
C---- KWACK       as of 2006 Mar 14
      integer     MWKS,NWKS,KSWPR,KSWSH
      parameter   (MWKS=27)
      logical     KWKS
      dimension   KWKS(MWKS),KSWPR(MWKS),KSWSH(MWKS)
C     (Need to revise     TURKOIS     when changing MWKS ! )
      common      /KWACK1/ NWKS,KWKS
      common      /KWACK2/ KSWPR
      common      /KWACK3/ KSWSH
C---- Codes describing "continuum" wavelengths
C
C      1: regular (constant background) line center, printed;
C      2: "additional" wavelength, no Eclipse;
C      3: "additional" wavelength, with Eclipse;
C      4: line source function background, PRD;
C      5: rates integrations, regular;
C      6: additional photoionization;
C      7: H- calculation;
C      8: dust temperature adjustment procedure;
C      9: HSE calculation;
C     10: "Lyman" calculation (level-K-to-continuum integration);
C     11: incident coronal radiation;
C     12: rates integrations, K-shell;
C     13: composite line opacity, no Eclipse;
C     14: miscellaneous;
C     15: composite line opacity, with Eclipse;
C     16: line source function background, FDB;
C     17: actual CO-lines opacity, fundamental;
C     18: FDB line center, printed;
C     19: regular (constant background) line center, not printed;
C     20: actual CO-lines opacity, first overtone;
C     21: actual CO-lines opacity, band limit;
C     22: actual CO-lines opacity, rotational;
C     23: actual CO-lines opacity, second overtone.
C     24: PRD line center, printed;
C     25: PRD line center, not printed;
C     26: FDB line center, not printed;
C     27: standard background.
C     .
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 6),NSW)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(239),JJSCW)
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
      equivalence (IQQ(200),IQPFC)
      equivalence (IQQ( 24),IQOST)
      equivalence (IQQ( 52),IQCDP)
      equivalence (IQQ( 37),IQAPC)
      equivalence (IQQ( 65),IQCPL)
      equivalence (IQQ( 11),IQCPA)
      equivalence (IQQ( 69),IQHMP)
      equivalence (IQQ(100),IQDTP)
      equivalence (IQQ(115),IQLYC)
      equivalence (IQQ(171),IQCPC)
      equivalence (IQQ(181),IQKSP)
      equivalence (IQQ(224),IQCCP)
      equivalence (IQQ(300),IQTCP)
      equivalence (IQQ(322),IQSBP)
C     !DASH
C     !EJECT
      external EQUAL, ZEUS, HI, BYE
C
      dimension X(*)
C
      data DELTA /1.D-8/
C
      call HI ('BUPPI')
C     !BEG
      PRNT = .false.
C
      if(KTRU.eq.1) then
        PRNT = IQTCP.gt.0
      else
        if(NSW.gt.0) then
          call EQUAL (X(JJSCW), NSW, DELTA, XLM, I)
          PRNT = I.gt.0
        end if
      end if
C
      if((.not.PRNT).and.(KTRU.ne.1)) then
        call ZEUS    (IQOST, IQCDP, J5 )
        call ZEUS    (IQOST, IQAPC, J6 )
        call ZEUS    (IQOST, IQSBP, J27)
        JFDB = IQPFC+IQCPL
C
        PRNT = (KWKS( 1).and.(IQCPL.gt.0)) .or.
     $         (KWKS( 2).and.(IQCPA.gt.0)) .or.
     $         (KWKS( 3).and.(IQCPA.gt.0)) .or.
     $         (KWKS( 5).and.(J5   .gt.0)) .or.
     $         (KWKS( 6).and.(J6   .gt.0)) .or.
     $         (KWKS( 7).and.(IQHMP.gt.0)) .or.
     $         (KWKS( 8).and.(IQDTP.gt.0)) .or.
     $         (KWKS(10).and.(IQLYC.gt.0)) .or.
     $         (KWKS(11).and.(IQLYC.gt.0)) .or.
     $         (KWKS(12).and.(IQKSP.gt.0)) .or.
     $         (KWKS(13).and.(IQCPC.gt.0)) .or.
     $         (KWKS(15).and.(IQCPC.gt.0)) .or.
     $         (KWKS(16).and.(IQPFC.gt.0)) .or.
     $         (KWKS(17).and.(IQCCP.gt.0)) .or.
     $         (KWKS(18).and.(JFDB .gt.0)) .or.
     $         (KWKS(19).and.(IQCPL.gt.0))
        PRNT = PRNT .or.
     $         (KWKS(20).and.(IQCCP.gt.0)) .or.
     $         (KWKS(21).and.(IQCCP.gt.0)) .or.
     $         (KWKS(22).and.(IQCCP.gt.0)) .or.
     $         (KWKS(23).and.(IQCCP.gt.0)) .or.
     $         (KWKS(26).and.(JFDB .gt.0)) .or.
     $         (KWKS(27).and.(J27  .gt.0))
      end if
C     !END
      call BYE ('BUPPI')
C
      return
      end
