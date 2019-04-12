      subroutine OLINDA
     $(RU,RO,RN,WEIGHT,N,IFS,ILS,WSM,IU,IL,WEIT)
C
C     Rudolf Loeser, 1977 Feb 03
C---- Supervises final RHO smoothing and weighting.
C     !DASH
      save
C     !DASH
      real*8 ONE, RN, RO, RU, WEIGHT, WEIT, WSM
      integer IFS, IL, ILS, IQWSR, IU, KMSS, KWSS, MODE, N
      character TITLE*10
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
      equivalence (IQQ(123),IQWSR)
C     !DASH
      external KERMESS, CAPRA, SMOG, WEITER, HI, BYE
C
C               RU(N), RO(N), RN(N), WEIGHT(N), WEIT(N)
      dimension RU(*), RO(*), RN(*), WEIGHT(*), WEIT(*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('OLINDA')
C     !BEG
      call KERMESS ('NO', KMSS)
      call CAPRA   (      KWSS)
C
      write (TITLE,100) IU,IL
  100 format('RHO(',I2,'/',I2,')')
C
      call SMOG    (RN, N, IFS, ILS, WSM, KMSS, TITLE)
C
      call WEITER  (RU, RN, RO, WEIGHT, ONE, N, IQWSR, MODE, KWSS,
     $              TITLE, WEIT)
C     !END
      call BYE ('OLINDA')
C
      return
      end
