      subroutine SPUMONI
     $(ARG,MODE,X,CNDT)
C
C     Rudolf Loeser, 1970 Feb 27
C---- Computes the incident radiation term table.
C     !DASH
      save
C     !DASH
      real*8 ARG, CNDT, X
      integer I, IQINC, JJFIN, JJXIN, JJZ, MODE, N
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
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
      external  SPUME, ZERO1, HI, BYE
C
      dimension X(*)
C
C               CNDT(N)
      dimension CNDT(*)
C     !EJECT
C
      call HI ('SPUMONI')
C     !BEG
      if(IQINC.gt.0) then
C
        do 100 I = 1,N
          call SPUME (ARG,I,X(JJZ),MODE,1,X(JJXIN),X(JJFIN), CNDT(I))
  100   continue
C
      else
C
        call ZERO1   (CNDT,N)
C
      end if
C     !END
      call BYE ('SPUMONI')
C
      return
      end
