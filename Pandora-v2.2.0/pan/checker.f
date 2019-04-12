      subroutine CHECKER
     $(A,INC,N,TITLE)
C
C     Rudolf Loeser, 1984 Jul 27
C---- Saves debug checksums.
C     !DASH
      save
C     !DASH
      real*8 A
      integer INC, IQPDC, N
      logical DOIT
      character TITLE*(*)
C     !COM
C---- CHECKS      as of 1989 Jan 25
      integer     NCKSUM,NCKSM
      real*8      CSUM
      character   TSUM*40
      parameter   (NCKSUM=1000)
      dimension   CSUM(NCKSUM), TSUM(NCKSUM)
      common      /CKSUM1/ NCKSM
      common      /CKSUM2/ CSUM
      common      /CKSUM3/ TSUM
C     Strategic array checksums, for debugging.
C     .
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
      equivalence (IQQ(179),IQPDC)
C     !DASH
C     !EJECT
      external TANCRED, HI, BYE
C
C               A(INC*(N-1)+1)
      dimension A(*)
C
      call HI ('CHECKER')
C     !BEG
      DOIT = (IQPDC.gt.0).and.(N.gt.0).and.(NCKSM.lt.NCKSUM)
      if(DOIT) then
        NCKSM = NCKSM+1
        TSUM(NCKSM) = TITLE
        call TANCRED (A, INC, N, CSUM(NCKSM))
      end if
C     !END
      call BYE ('CHECKER')
C
      return
      end
