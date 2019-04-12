      subroutine HEDRU
     $(X,FRS,HND,VXS,VEC,RML)
C
C     Rudolf Loeser, 1983 May 19
C---- Sets up mass loss rate.
C     !DASH
      save
C     !DASH
      real*8 FRS, HND, RML, VEC, VXS, X
      integer IQEXA, N
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
      equivalence (IQQ(169),IQEXA)
C     !DASH
      external AUTUN, HI, BYE
C
      dimension X(*)
C
C               FRS(N), HND(N), VXS(N), RML(N), VEC(N)
      dimension FRS(*), HND(*), VXS(*), RML(*), VEC(*)
C     !EJECT
C
      call HI ('HEDRU')
C     !BEG
      if(IQEXA.gt.0) then
        call AUTUN (N,X,FRS,HND,VXS,VEC,RML)
      end if
C     !END
      call BYE ('HEDRU')
C
      return
      end
