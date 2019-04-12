      subroutine ANATINI
     $(LUMR,XLB1,AIJ,CP,CII,CEIJ,WRAT,RRCP,MRJ,XNU,P)
C
C     Rudolf Loeser, 1992 Jan 13
C---- Controls saving of computed atomic data.
C     (This is version 2 of ANATINI.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CEIJ, CII, CP, P, RRCP, WRAT, XLB1, XNU, XNUK
      integer IOMX, IOVER, IQDAS, JATAW, JDAIJ, JDCEA, JDCEG, JDCEI,
     $        JDCEJ, JDCER, JDCES, JDCEV, JDCEW, JDCIC, JDCIJ, JDCIV,
     $        JDCPI, JDCRD, JDCRS, JDCSK, JDCVW, JDNUK, JDPSW, JDRCP,
     $        JDXNU, LUMR, MRJ, NL, NT, NTE
      logical FA, FANY, FCEIJ, FCII, FCP, FCRD, FCRS, FCSK, FCVW, FLEV,
     $        FNUK, FP, FRRCP, FTRAN, FWRAT, FXNU
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(20),NTE)
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ(  8),IOMX )
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ(196),JATAW)
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
      equivalence (LEST( 2),IOVER)
C     !EJECT
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
C     .
      equivalence
     $(MEST( 1),JDCES),(MEST( 2),JDCEG),(MEST( 3),JDCEI),
     $(MEST( 4),JDCEV),(MEST( 5),JDCEJ),(MEST( 6),JDCIV),
     $(MEST( 7),JDCIJ),(MEST( 8),JDCRD),(MEST( 9),JDAIJ),
     $(MEST(10),JDCVW),(MEST(11),JDCSK),(MEST(12),JDCRS),
     $(MEST(13),JDCPI),(MEST(14),JDRCP),(MEST(15),JDCER),
     $(MEST(16),JDXNU),(MEST(17),JDNUK),(MEST(18),JDPSW),
     $(MEST(19),JDCEA),(MEST(20),JDCIC),(MEST(21),JDKNT),
     $(MEST(22),JDCIA),(MEST(23),JDCP1),(MEST(24),JDRP1),
     $(MEST(25),JDCIS),(MEST(26),JDCIW),(MEST(27),JDCPN),
     $(MEST(28),JDCEW)
C     .
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
      equivalence (IQQ(293),IQDAS)
C     !DASH
C     !EJECT
      external YARDEN, BARBET, BURBOT, HI, BYE
C
C               WRAT(MRS), CEIJ(NTE,MUL), XLB1(Li1len), CII(NTE,NSL+1),
      dimension WRAT(*),   CEIJ(*),       XLB1(*),      CII(*),
C
C               AIJ(NL,NL), CP(NSL+1), RRCP(MRS), MRJ(NSL+1), XNU(NSL),
     $          AIJ(*),     CP(*),     RRCP(*),   MRJ(*),     XNU(*),
C
C               P(NSL)
     $          P(*)
C
      call HI ('ANATINI')
C     !BEG
      if((IQDAS.gt.0).and.(IOVER.eq.IOMX)) then
        FA    = (JDAIJ.gt.0)
        FCRD  = (JDCRD.gt.0)
        FCVW  = (JDCVW.gt.0)
        FCSK  = (JDCSK.gt.0)
        FCRS  = (JDCRS.gt.0)
        FCEIJ = (JDCES.gt.0).or.(JDCEG.gt.0).or.(JDCEI.gt.0).or.
     $          (JDCEV.gt.0).or.(JDCEJ.gt.0).or.(JDCER.gt.0).or.
     $          (JDCEW.gt.0)
        FCP   = (JDCPI.gt.0)
        FCII  = (JDCIV.gt.0).or.(JDCIJ.gt.0).or.(JDCIC.gt.0)
        FWRAT = (JATAW.gt.0)
        FRRCP = (JDRCP.gt.0).or.JATAW
        FP    = (JDPSW.gt.0)
        FXNU  = (JDXNU.gt.0)
        FNUK  = (JDNUK.gt.0)
C
        FTRAN = FA.or.FCRD.or.FCVW.or.FCSK.or.FCRS.or.FCEIJ
        FLEV  = FCP.or.FCII.or.FWRAT.or.FRRCP.or.FP.or.FXNU.or.FNUK
        FANY  = FTRAN.or.FLEV
C
        if(FANY) then
          call YARDEN   (LUMR, 1, 'ATOM')
          if(FLEV) then
C----       Levels stuff
            call BARBET (LUMR, CP, FCP, CII, FCII, RRCP, MRJ, FRRCP,
     $                   WRAT, FWRAT, P, FP, XNU, FXNU, XNUK, FNUK)
          end if
          if(FTRAN) then
C----       Transitions stuff
            call BURBOT (LUMR, NT, NL, NTE, AIJ, FA, CEIJ, FCEIJ, XLB1,
     $                   FCRD, FCVW, FCSK, FCRS)
          end if
          call YARDEN   (LUMR, 2, 'ATOM')
        end if
      end if
C     !END
      call BYE ('ANATINI')
C
      return
      end
