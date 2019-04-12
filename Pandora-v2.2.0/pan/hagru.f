      subroutine HAGRU
     $(YL,YPRE,YCONT,NT,YLYM,KK,YCR,NCR,YKRJ,MLS,YRATE,MRS)
C
C     Rudolf Loeser, 1985 Feb 14
C---- Edits Source Function method control switches for consistency, and
C     establishes Source Function methods summaries.
C
C     Part II  --  see also HUGRU.
C     !DASH
      save
C     !DASH
      real*8 YCONT, YCR, YKRJ, YL, YLYM, YPRE, YRATE
      integer IQLYM, IQUTR, KK, MLS, MRS, MSFQM, NCR, NT
      logical KILROY
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(18),MSFQM)
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
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ( 53),IQUTR)
C     !DASH
C     !EJECT
      external EROLIA, GUENON, MASHED, HI, BYE
C
C               YCONT(NT), YLYM(KK), YCR(NCR), YKRJ(MLS), YRATE(MRS)
      dimension YCONT(*),  YLYM(*),  YCR(*),   YKRJ(*),   YRATE(*)
C
      data KILROY /.true./
C
      call HI ('HAGRU')
C     !BEG
      call GUENON   (KILROY, 'HAGRU')
      call EROLIA   (YCONT,NT ,'YCONT', KILROY, 'HAGRU')
C
      if(IQLYM.gt.0) then
        call EROLIA (YL   ,1  ,'YL'   , KILROY, 'HAGRU')
        call EROLIA (YLYM ,KK ,'YLYM' , KILROY, 'HAGRU')
      end if
C
      call EROLIA   (YCR  ,NCR,'YCR'  , KILROY, 'HAGRU')
      call EROLIA   (YKRJ ,MLS,'YKRJ' , KILROY, 'HAGRU')
C
      if(IQUTR.le.0) then
        call EROLIA (YRATE,MRS,'YRATE', KILROY, 'HAGRU')
      end if
C
      if(MSFQM.gt.0) then
        call EROLIA (YPRE ,1  ,'YPRE' , KILROY, 'HAGRU')
      end if
C
      if(.not.KILROY) then
        call MASHED ('HAGRU')
      end if
C     !END
      call BYE ('HAGRU')
C
      return
      end
