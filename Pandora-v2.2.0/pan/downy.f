      subroutine DOWNY
     $(LFB,EMU,INCRAD)
C
C     Rudolf Loeser, 1978 May 28
C---- Computes incident radiation flag for Spectrum calculations.
C     LFB=1 means: front-face emergent radiation;
C        =2 means: back-face emergent radiation.
C     !DASH
      save
C     !DASH
      real*8 EMU, ONE
      integer IQEBI, IQIFF, IQINC, LFB, LFBSAV
      logical GEOM, INCRAD, KILROY, KINCAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
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
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ(208),IQEBI)
C     !DASH
      external HI, BYE
C
      data KILROY /.true./
      data LFBSAV /-1/
C     !EJECT
C
      call HI ('DOWNY')
C     !BEG
      if(KILROY.or.(LFB.ne.LFBSAV)) then
        LFBSAV = LFB
C
        GEOM = .false.
        if(LFB.eq.1) then
          GEOM = IQIFF.le.0
        else if(LFB.eq.2) then
          GEOM = IQIFF.gt.0
        end if
C
        KINCAD = (IQINC.gt.0).and.GEOM
      end if
C
      if(KILROY) then
        KILROY = .false.
      end if
C
      INCRAD = (EMU.eq.ONE).and.KINCAD
C     !END
      call BYE ('DOWNY')
C
      return
      end
