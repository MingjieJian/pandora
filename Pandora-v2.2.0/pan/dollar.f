      subroutine DOLLAR
     $(X,W,XLB1)
C
C     Rudolf Loeser, 2000 Jan 03
C---- Saves transition data in special file.
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1
      integer IQSTD, JJTE, JJZ, LUSF, MMJBR, MMRHO, MMS, MMST, MMTAU, N
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(21),LUSF )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(26),MMS  )
      equivalence (MML(16),MMTAU)
      equivalence (MML(46),MMST )
      equivalence (MML(27),MMJBR)
      equivalence (MML(28),MMRHO)
C     !EJECT
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
      equivalence (IQQ( 10),IQSTD)
C     !DASH
      external TALER, HI, BYE
C
      dimension X(*), W(*)
C
C               XLB1(Li1Len)
      dimension XLB1(*)
C
      data KILROY /.true./
C
      call HI ('DOLLAR')
C     !BEG
      if(IQSTD.gt.0) then
        call TALER (KILROY,LUSF,N,X(JJZ),X(JJTE),XLB1(MMTAU),
     $              XLB1(MMS),XLB1(MMRHO),XLB1(MMJBR),XLB1(MMST))
      end if
C     !END
      call BYE ('DOLLAR')
C
      return
      end
