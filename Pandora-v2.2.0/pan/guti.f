      subroutine GUTI
     $(P,FRS,S,N)
C
C     Rudolf Loeser, 1983 Jul 14
C---- Applies the factor R**2 if needed.
C     !DASH
      save
C     !DASH
      real*8 FRS, P, S
      integer IQSFS, N
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
      equivalence (IQQ( 31),IQSFS)
C     !DASH
      external OXUS, MOVE1, HI, BYE
C
C               P(N), FRS(N), S(N)
      dimension P(*), FRS(*), S(*)
C
      call HI ('GUTI')
C     !BEG
      if(IQSFS.gt.0) then
        call OXUS  (P,FRS,S,N)
      else
        call MOVE1 (P,N,S)
      end if
C     !END
      call BYE ('GUTI')
C
      return
      end
