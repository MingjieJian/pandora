      subroutine WADDY
     $(COL,N,XCBL,FREQ,WAVE,VEC,YNT)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Computes actual CO-lines cooling rate.
C     !DASH
      save
C     !DASH
      real*8 COL, FREQ, VEC, WAVE, XCBL, YNT
      integer IQCRD, N, NF
      logical DMP
      character TIT*22
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
      equivalence (IQQ(226),IQCRD)
C     !DASH
      external BASSIA, CARAWAY, HI, BYE
C
C               YNT(N,Numkon), FREQ(Numkon), WAVE(Numkon), XCBL(Miklen),
      dimension YNT(*),        FREQ(*),      WAVE(*),      XCBL(*),
C
C               VEC(N), COL(N)
     $          VEC(*), COL(*)
C
      data TIT /'CO-lines cooling rates'/
C
      call HI ('WADDY')
C     !BEG
C---- Get data from Continuum Data blocks
      call BASSIA  (XCBL,N,NF,FREQ,WAVE,VEC,YNT)
C---- Integrate (with optional dump)
      DMP = IQCRD.gt.0
      call CARAWAY (YNT,NF,N,FREQ,WAVE,COL,TIT,DMP)
C     !END
      call BYE ('WADDY')
C
      return
      end
