      subroutine WISENT
     $(XRAY,N,XCBL,FREQ,WAVE,VEC,YNT)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Computes X-rays cooling rate.
C     !DASH
      save
C     !DASH
      real*8 FREQ, VEC, WAVE, XCBL, XRAY, YNT
      integer IQXRI, N, NF
      logical DMP
      character TIT*19
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
      equivalence (IQQ(197),IQXRI)
C     !DASH
      external PASANG, CARAWAY, HI, BYE
C
C               FREQ(Numkon), WAVE(Numkon), YNT(N,Numkon), XCBL(Miklen),
      dimension FREQ(*),      WAVE(*),      YNT(*),        XCBL(*),
C
C               XRAY(N), VEC(N)
     $          XRAY(*), VEC(*)
C
      data TIT /'X-ray cooling rates'/
C
      call HI ('WISENT')
C     !BEG
C---- Get data from Continuum Data blocks
      call PASANG  (XCBL,N,NF,FREQ,WAVE,VEC,YNT)
C---- Integrate (with optional dump)
      DMP = IQXRI.gt.0
      call CARAWAY (YNT,NF,N,FREQ,WAVE,XRAY,TIT,DMP)
C     !END
      call BYE ('WISENT')
C
      return
      end
