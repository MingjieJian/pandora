      subroutine GYGES
     $(LU,N,KOLEV,BDX,BDXEP,CHECKL)
C
C     Rudolf Loeser, 1999 Dec 29
C---- Checks BDXEP against BDX, for Lyman calculation.
C     (This is version 2 of GYGES.)
C     !DASH
      save
C     !DASH
      real*8 BDX, BDXEP, CHECKL
      integer IQCGP, IQLYA, KOLEV, LU, N
      character LA*3, LB*7
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
      equivalence (IQQ(251),IQCGP)
      equivalence (IQQ(255),IQLYA)
C     !DASH
C     !EJECT
      external ABJECT, LINER, ZERO1, TAKE, HI, BYE
C
C               BDX(N), BDXEP(N), CHECKL(N)
      dimension BDX(*), BDXEP(*), CHECKL(*)
C
      call HI ('GYGES')
C     !BEG
      call ZERO1    (CHECKL, N)
C
      if((IQCGP.gt.0).and.(LU.gt.0).and.(IQLYA.le.0)) then
        write (LA,100) KOLEV
  100   format('BD',I1)
        write (LB,101) KOLEV
  101   format('BD',I1,'EP')
C
        call ABJECT (LU)
        write (LU,102) LA,LB
  102   format(' ','Consistency check',94X,'(Option CHKPRNT)'//
     $         ' ','CHECKL = Ratio = ',A,' / ',A)
        call LINER  (2, LU)
        call TAKE   (LU, N, BDX, LA, BDXEP, LB, CHECKL)
      end if
C     !END
      call BYE ('GYGES')
C
      return
      end
