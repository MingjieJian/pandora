      subroutine ATOLL
     $(X,W,IW)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Computes partition function ratios.
C     (This is version 3 of ATOLL.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQPPF, IQUVP, IW, JJPFT, JJTE, JJXNE, JJZ, LU, N, NO
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
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(142),JJPFT)
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
      equivalence (IQQ(164),IQPPF)
      equivalence (IQQ(165),IQUVP)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external ZEUS, PRIAM, CORAL, REEF, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C
      call HI ('ATOLL')
C     !BEG
      call ZEUS    (NO,IQPPF,LU)
      call PRIAM   (LU,'PARTITION',9)
C
      if(IQUVP.gt.0) then
        call CORAL (N,X(JJTE),X(JJXNE),X(JJZ),X(JJPFT),W,IW,LU)
      else
        call REEF  (N,X(JJPFT),LU)
      end if
C
C     !END
      call BYE ('ATOLL')
C
      return
      end
