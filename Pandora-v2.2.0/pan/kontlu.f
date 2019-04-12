      subroutine KONTLU
     $(NW,WAVES,EMINT,YSTAR)
C
C     Rudolf Loeser, 2005 Dec 21
C---- Saves the computed, mu = 1, background spectrum, for use with
C     emergent line profile calculation.
C     (This is version 2 of KONTLU.)
C     !DASH
      save
C     !DASH
      real*8 EMINT, WAVES, YSTAR
      integer IQPPU, NW
C     !COM
C---- CHICLE      as of 2005 Dec 21
      integer     LIMBG,NUMBG
      parameter   (LIMBG=10000)
      real*8      BWAV,BIHZ,BIAN
      dimension   BWAV(LIMBG),BIHZ(LIMBG),BIAN(LIMBG)
      common      /CHICLE1/ NUMBG
      common      /CHICLE2/ BWAV
      common      /CHICLE3/ BIHZ
      common      /CHICLE4/ BIAN
C     Computed background spectrum at "non-line" wavelength
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
      equivalence (IQQ(151),IQPPU)
C     !DASH
C     !EJECT
      external  MOVE1, HI, BYE
      intrinsic min
C
C               WAVES(Numkon), EMINT(Numkon), YSTAR(Numkon)
      dimension WAVES(*),      EMINT(*),      YSTAR(*)
C
      call HI ('KONTLU')
C     !BEG
      if(IQPPU.gt.0) then
        NUMBG = min(NW,LIMBG)
        if(NUMBG.gt.0) then
          call MOVE1 (WAVES, NUMBG, BWAV)
          call MOVE1 (EMINT, NUMBG, BIHZ)
          call MOVE1 (YSTAR, NUMBG, BIAN)
        end if
      end if
C     !END
      call BYE ('KONTLU')
C
      return
      end
