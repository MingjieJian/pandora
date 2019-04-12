      subroutine TWENTY
     $(TAU,TS)
C
C     Rudolf Loeser, 1970 Feb 16
C---- Controls ONE.
C     !DASH
      save
C     !DASH
      real*8 TAU, TS
      integer IQLSC, LU, MO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ( 89),IQLSC)
C     !DASH
      external ZEUS, ONE, HI, BYE
C
C               TAU(N,NT), TS(M)
      dimension TAU(*),    TS(*)
C
      call HI ('TWENTY')
C     !BEG
      call ZEUS  (MO,IQLSC,LU)
      if(LU.gt.0) then
        call ONE (TS,TAU,LU)
      end if
C     !END
      call BYE ('TWENTY')
C
      return
      end
