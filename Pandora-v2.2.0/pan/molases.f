      subroutine MOLASES
     $(NO,KIBI,N)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Prints an iteration trend summary.
C     (This is version 2 of MOLASES.)
C     !DASH
      save
C     !DASH
      integer I, IQSMT, J, K, KIBI, KNUM, N, NO
      character PERIOD*1
C     !COM
C---- SOLO        as of 2001 Aug 28
      integer     NLINSO,KLENSO
      parameter   (NLINSO=250)
      parameter   (KLENSO=108)
C     (REMEMBER to recompile JAY when changing these parameters!)
      integer     LNTRSO
      parameter   (LNTRSO=NLINSO*KLENSO)
      character   TREND*(LNTRSO), WSOLO*(KLENSO)
      common      /SOLO/ TREND,WSOLO
C     Working storage for Iterative Summary.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
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
      equivalence (IQQ(284),IQSMT)
C     !DASH
C     !EJECT
      external  DEVERON, HI, BYE
      intrinsic min
C
      call HI ('MOLASES')
C     !BEG
      if((NO.gt.0).and.(KIBI.gt.0).and.(IQSMT.gt.0)) then
C
        call DEVERON (NO)
C
        KNUM = min(((N/10)+1),9)
        KNUM = 10*KNUM
        write (NO,100) (I,I=10,KNUM,10)
  100   format(' ',19X,9(10X,I2))
        write (NO,101) (PERIOD,I=1,KNUM)
  101   format(' ',19X,18(A1,A1,A1,A1,A1,' '))
C
        K = -KLENSO
        do 103 J = 1,KIBI
          K = K+KLENSO
          WSOLO = TREND(K+1:K+110)
          write (NO,102) WSOLO(1:18),(WSOLO(I+9:I+13),WSOLO(I+14:I+18),
     $                            I=10,KNUM,10)
  102     format(' ',A18,1X,18(A5,' '))
  103   continue
C
      end if
C     !END
      call BYE ('MOLASES')
C
      return
      end
