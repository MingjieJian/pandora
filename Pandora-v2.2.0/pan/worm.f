      subroutine WORM
     $(RLINS,N,XCBL,FREQ,WAVE,VEC,YNT)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Computes Composite Lines cooling rate.
C     (This is version 4 of WORM.)
C     !DASH
      save
C     !DASH
      real*8 FREQ, RLINS, VEC, WAVE, XCBL, YNT
      integer IQKLD, N, NF
      logical DMP
      character TIT*29
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
      equivalence (IQQ(195),IQKLD)
C     !DASH
      external PARSLEY, CARAWAY, HI, BYE
C
C               FREQ(Numkon), YNT(N,Numkon), WAVE(Numkon), XCBL(Miklen),
      dimension FREQ(*),      YNT(*),        WAVE(*),      XCBL(*),
C
C               RLINS(N), VEC(N)
     $          RLINS(*), VEC(*)
C
      data TIT /'Composite Lines cooling rates'/
C
      call HI ('WORM')
C     !BEG
C---- Get data from Continuum Data blocks
      call PARSLEY (XCBL,N,NF,FREQ,WAVE,VEC,YNT)
C---- Integrate (with optional dump)
      DMP = IQKLD.gt.0
      call CARAWAY (YNT,NF,N,FREQ,WAVE,RLINS,TIT,DMP)
C     !END
      call BYE ('WORM')
C
      return
      end
