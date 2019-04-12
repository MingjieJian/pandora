      subroutine WHIRL
     $(X,W,IW,TAU,Z,N,YDAMP,WN,TITLE,KODE)
C
C     Rudolf Loeser, 1981 Nov 13.
C---- Set up Weight Matrix WN for Continuum Source Function calculation.
C     Returns with KODE=1 if all seems OK, =0 if not.
C     (This is version 2 of WHIRL.)
C     !DASH
      save
C     !DASH
      real*8 TAU, W, WN, X, YDAMP, Z
      integer IQFIN, IW, KODE, LUEO, N
      logical FIN, GDIL, TAURED
      character TITLE*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
C     !EJECT
      external LAMBDA, MESHED, MASHED, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), Z(N), WN(N,N)
      dimension TAU(*), Z(*), WN(*)
C
      data TAURED,GDIL /.true., .true./
C
      call HI ('WHIRL')
C     !BEG
      FIN = IQFIN.gt.0
      call LAMBDA   (X, W, IW, TAU, N, N, YDAMP, FIN, TAURED, GDIL,
     $               KODE, TITLE, WN)
C
      if(KODE.le.0) then
        call MESHED ('WHIRL', 3)
        write (LUEO,100) TITLE
  100   format(' ','Matrix computation failed.'/
     $         ' ',A)
        call MASHED ('WHIRL')
      end if
C     !END
      call BYE ('WHIRL')
C
      return
      end
