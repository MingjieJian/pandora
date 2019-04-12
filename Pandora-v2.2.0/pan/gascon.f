      subroutine GASCON
     $(X,XLB1,JTRANS)
C
C     Rudolf Loeser, 1998 Jun 04
C---- Sets up "artificial TAU" for RHO/W calculation.
C     !DASH
      save
C     !DASH
      real*8 X, XLB1
      integer IQRWO, JJTRA, JTRANS, MMTAU
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(173),JJTRA)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(16),MMTAU)
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
      equivalence (IQQ(204),IQRWO)
C     !DASH
C     !EJECT
      external BICHIR, HI, BYE
C
      dimension X(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      call HI ('GASCON')
C     !BEG
      if(IQRWO.gt.0) then
        call BICHIR (XLB1(MMTAU),X(JJTRA),JTRANS)
      end if
C     !END
      call BYE ('GASCON')
C
      return
      end
