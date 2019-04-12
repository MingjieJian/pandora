      subroutine AKKAD
     $(X,W,IW)
C
C     Rudolf Loeser, 1981 Mar 31
C---- Controls computation of Lambda-Operator for standard TAU table.
C     !DASH
      save
C     !DASH
      real*8 W, X, YPRE
      integer IQFIN, IW, JJTS, JJXM, KODE, M
      logical FIN, GDIL, TAURED
      character TITLE*16
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ( 95),JJXM )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 20),YPRE )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external LAMBDA, HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      data TITLE  /'Precalculated XM'/
      data TAURED /.false./
      data FIN    /.false./
      data GDIL   /.true./
C
      call HI ('AKKAD')
C     !BEG
      if(IQFIN.le.0) then
        call LAMBDA (X, W, IW, X(JJTS), M, M, YPRE, FIN, TAURED, GDIL,
     $               KODE, TITLE, X(JJXM))
      else
        KODE = 0
      end if
C
      if(KODE.ne.1) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,'; trouble computing standard XM matrix.')
        call HALT ('AKKAD', 1)
      end if
C     !END
      call BYE ('AKKAD')
C
      return
      end
