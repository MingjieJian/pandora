      subroutine JAY
     $(N,ITMX,A,TIT,KIBI)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Makes an Iteration Trend Summary, and saves it in TREND.
C     (This is version 2 of JAY.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, IQSMT, ITMX, J, JA, JB, K, KBL, KIBI, KODE, N, NN
      character BLANK*1, TIT*15
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
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
      external  DILLY, MAGPIE, HI, BYE
      intrinsic min
C
C               A(N,ITMX)
      dimension A(N,*)
C
      call HI ('JAY')
C     !BEG
      if(IQSMT.gt.0) then
        KIBI = KIBI+1
        if(KIBI.le.NLINSO) then
C
C----     Do this only because there is room left in TREND
          K = KLENSO*(KIBI-1)
C----     Initialize array indices
          JA = ITMX-2
          JB = ITMX-1
C----     Set depths limit
          NN = min(N,90)
C----     Initialize BLANKs counter
          KBL = 0
C----     Initialize current line's character position
          J = K+18
C----     Loop over depths
          do 100 I = 1,NN
            J = J+1
C----       Produce iteration trend code for this depth
            call MAGPIE (A(I,JA), A(I,JB), TREND(J:J), KODE)
            KBL = KBL+KODE
  100     continue
C
          if(KBL.lt.NN) then
C----       Trend summary line not all blanks - accept it
            TREND(K+1:K+13) = TIT
C----       Add depth-of-greatest-change
            call DILLY  (A(1,JB), N, TREND(K+14:K+18))
          else
C----       Trend summary line is all blanks - reject it
            KIBI = KIBI-1
          end if
C
        end if
      end if
C     !END
      call BYE ('JAY')
C
      return
      end
