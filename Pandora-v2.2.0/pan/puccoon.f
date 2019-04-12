      subroutine PUCCOON
     $(N,CSF,BTE,BTR,BC)
C
C     Rudolf Loeser, 1995 May 05
C---- Computes BC, a radiation function required for the
C     Line Source Function calculation for transition (IU,IL).
C     (This is version 2 of PUCCOON.)
C     !DASH
      save
C     !DASH
      real*8 BC, BTE, BTR, CSF
      integer IQCSB, IQCSF, IQCSW, N
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
      equivalence (IQQ( 46),IQCSF)
      equivalence (IQQ(166),IQCSB)
      equivalence (IQQ( 14),IQCSW)
C     !DASH
C     !EJECT
      external  ZAFRA, MOVE1, HI, BYE
C
C               CSF(N), BTE(N), BTR(N), BC(N)
      dimension CSF(*), BTE(*), BTR(*), BC(*)
C
      call HI ('PUCCOON')
C     !BEG
      if(IQCSF.gt.0) then
        if(IQCSB.gt.0) then
          call ZAFRA (CSF, BTE, N, BC)
        else
          call MOVE1 (CSF,      N, BC)
        end if
      else
        if(IQCSW.gt.0) then
          call MOVE1 (BTR,      N, BC)
        else
          call MOVE1 (BTE,      N, BC)
        end if
      end if
C     !END
      call BYE ('PUCCOON')
C
      return
      end
