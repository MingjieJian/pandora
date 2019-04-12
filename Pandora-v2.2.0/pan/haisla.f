      subroutine HAISLA
     $(RKB,RKA,RCHECK,N)
C
C     Rudolf Loeser, 1990 Mar 02
C---- Computes RCHECK, and prints it, for FAKE.
C     (This is version 2 of HAISLA.)
C     !DASH
      save
C     !DASH
      real*8 RCHECK, RKA, RKB
      integer IQCGP, IQCRK, IQLYA, LU, MO, N
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
      equivalence (IQQ(126),IQCRK)
      equivalence (IQQ(251),IQCGP)
      equivalence (IQQ(255),IQLYA)
C     !DASH
C     !EJECT
      external ZEUS, ABJECT, ZERO1, LINER, TAKE, HI, BYE
C
C               RKB(N), RKA(N), RCHECK(N)
      dimension RKB(*), RKA(*), RCHECK(*)
C
      call HI ('HAISLA')
C     !BEG
      call ZERO1    (RCHECK, N)
C
      call ZEUS     (MO, IQCRK, LU)
      if((IQCGP.gt.0).and.(LU.gt.0).and.(IQLYA.le.0)) then
        call ABJECT (LU)
        write (LU,100)
  100   format(' ','Consistency check',94X,'(Options CHKPRNT)'//
     $         ' ','RCHECK = Ratio = RKB / RKA')
        call LINER  (2, LU)
        call TAKE   (LU, N, RKB, 'RKB', RKA, 'RKA', RCHECK)
      end if
C     !END
      call BYE ('HAISLA')
C
      return
      end
