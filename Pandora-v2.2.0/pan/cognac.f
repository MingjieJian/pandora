      subroutine COGNAC
     $(X,W,IW,TAUIJ)
C
C     Rudolf Loeser, 1980 May 02
C---- Controls fancy display of TAU scales.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, W, X
      integer IQLSC, IQSCL, IW, JJTS, MO
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
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
      equivalence (IQQ( 89),IQLSC)
      equivalence (IQQ(  2),IQSCL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PRIAM, TWENTY, GROUND, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAUIJ(N,NUMTRN)
      dimension TAUIJ(*)
C     !EJECT
C
      call HI ('COGNAC')
C     !BEG
      if(((IQLSC.gt.0).or.(IQSCL.gt.0)).and.(MO.gt.0)) then
        call PRIAM  (MO,'TAU SCALES',10)
C----   Print horizontally-collated graph of scales
        call TWENTY (TAUIJ,X(JJTS))
C----   Print vertically-collated table of scales
        call GROUND (TAUIJ,X(JJTS),W,IW)
      end if
C     !END
      call BYE ('COGNAC')
C
      return
      end
