      subroutine ACRAGAS
     $(TDST)
C
C     Rudolf Loeser, 1987 Mar 03
C---- Writes out TDST for iterative summary.
C     (This is version 2 of ACRAGAS.)
C     !DASH
      save
C     !DASH
      real*8 TDST
      integer IQITD, KUST, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(213),IQITD)
C     !DASH
      external BERWYN, HI, BYE
C
C               TDST(N)
      dimension TDST(*)
C
      data KUST /13/
C
      call HI ('ACRAGAS')
C     !BEG
      if(IQITD.gt.0) then
        call BERWYN (KUST,'Acragas','TDST',0,0,TDST,N,.true.)
      end if
C     !END
      call BYE ('ACRAGAS')
C
      return
      end
