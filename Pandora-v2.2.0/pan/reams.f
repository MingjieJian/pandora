      subroutine REAMS
     $(N,NV,NH,NCK,NO,DOIT)
C
C     Rudolf Loeser, 2003 Apr 23
C---- Determines whether to do CHECK graphs, and prints header,
C     for FRAME.
C     !DASH
      save
C     !DASH
      integer IQCGR, N, NCK, NH, NO, NV
      logical DOIT
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
C     !DASH
      external PRIAM, LINER, TACKIT, HI, BYE
C     !EJECT
C
      call HI ('REAMS')
C     !BEG
      DOIT = IQCGR.gt.0
C
      if(NO.gt.0) then
        call PRIAM      (NO,'CHECKS',6)
        call LINER      (2,NO)
C
        if(DOIT) then
          if(NCK.gt.0) then
            write (NO,100)
  100       format(' ','Graphs of b-ratios consistency CHECKs appear ',
     $                 'because option CHKGRAF=on.'/
     $             ' ','(Note: only "interesting" graphs are printed.)')
            call TACKIT (N,NV,NH,DOIT)
          end if
        else
          write (NO,101)
  101     format(' ','No graphs of b-ratios consistency CHECKs are ',
     $               'printed, because option CHKGRAF=off.')
        end if
C
      else
        DOIT = .false.
      end if
C     !END
      call BYE ('REAMS')
C
      return
      end
