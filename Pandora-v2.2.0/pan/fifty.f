      subroutine FIFTY
     $(X,W,IW,N,K,BKPC,BBC,SCNU,XKCNU)
C
C     Rudolf Loeser, 1986 Jun 06
C---- Frequency-Dependent Background:
C     sets up KPC and BC for each frequency of the current transition;
C     and copies them into the SC and KC slots in the current
C     Line Intensity Data block.
C     (This is version 2 of FIFTY.)
C     !DASH
      save
C     !DASH
      real*8 BBC, BKPC, SCNU, W, X, XKCNU
      integer IADRS, IBTE, IBTR, IN, IQFDD, IS, ITYPE, IW, IWAVE, IWS,
     $        IXCBL, JJTE, JJTR, JN, K, MO, MOX, MUX, N, NNW, NW
      logical DUMP
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  8),JJTR )
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
      equivalence (IQQ(202),IQFDD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external NINETY, ASTARTE, BEER, MOVE1, MUGGER, TERUEL, WGIVE,
     $         IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               BKPC(N,K), XKCNU(N,NW), SCNU(N,NW), BBC(N,K)
      dimension BKPC(*),   XKCNU(*),    SCNU(*),    BBC(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IXCBL ),(IN( 2),IWAVE ),(IN( 3),IBTE  ),(IN( 4),IBTR  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),ITYPE ),(JN( 2),IADRS )
C
      call HI ('FIFTY')
C     !BEG
C     (Get, and allocate, W allotment)
      call NINETY   (IN, IS,  MOX, 'FIFTY')
      call TERUEL   (JN, IWS, MUX, 'FIFTY')
C
      DUMP = (IQFDD.gt.0).and.(MO.gt.0)
C---- Set up Continuum Data Blocks addresses
      call ASTARTE  (2, K, NW, W(IWAVE), IW(ITYPE), IW(IADRS))
C---- Get data from Continuum Data Blocks
      call BEER     (W(IXCBL), W(IWAVE), IW(IADRS), NW, X(JJTE),
     $               X(JJTR), W(IBTE), W(IBTR), BKPC, BBC)
C---- Copy data into Line Intensity Data block
      NNW = N*NW
      call MOVE1    (BBC,  NNW, SCNU )
      call MOVE1    (BKPC, NNW, XKCNU)
C
      if(DUMP) then
        call MUGGER (N, NW, XKCNU, SCNU, 'FIFTY')
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W , 'FIFTY')
      call IGIVE    (IW, 'FIFTY')
C     !END
      call BYE ('FIFTY')
C
      return
      end
