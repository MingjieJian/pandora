      subroutine HUGRU
     $(YWAVE,NWV,YHM,MHM,YLDT,NDT,BANDY,NAB)
C
C     Rudolf Loeser, 1991 Mar 28
C---- Edits Source Function method control switches for consistency,
C     and establishes Source Function methods summaries.
C
C     Part I  --  see also HAGRU.
C     !DASH
      save
C     !DASH
      real*8 BANDY, YHM, YLDT, YWAVE
      integer IQDT2, IQHMS, MHM, NAB, NDT, NWV
      logical KILROY
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
      equivalence (IQQ( 68),IQHMS)
      equivalence (IQQ( 99),IQDT2)
C     !DASH
C     !EJECT
      external EROLIA, MASHED, HI, BYE
C
C               YWAVE(NWV), YHM(MHM), YLDT(NDT), BANDY(NAB)
      dimension YWAVE(*),   YHM(*),   YLDT(*),   BANDY(*)
C
      data KILROY /.true./
C
      call HI ('HUGRU')
C     !BEG
      call EROLIA   (YWAVE, NWV, 'YWAVE', KILROY, 'HUGRU')
C
      if(IQHMS.gt.0) then
        call EROLIA (YHM  , MHM, 'YHM'  , KILROY, 'HUGRU')
      end if
C
      if(IQDT2.gt.0) then
        call EROLIA (YLDT , NDT, 'YLDT' , KILROY, 'HUGRU')
      end if
C
      call EROLIA   (BANDY, NAB, 'BANDY', KILROY, 'HUGRU')
C
      if(.not.KILROY) then
        call MASHED ('HUGRU')
      end if
C     !END
      call BYE ('HUGRU')
C
      return
      end
