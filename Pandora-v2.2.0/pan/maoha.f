      subroutine MAOHA
     $(NAB,KWC,KWA)
C
C     Rudolf Loeser, 1998 Nov 17
C---- Initializes some opacity data files, and processing.
C     !DASH
      save
C     !DASH
      integer IQALO, KOMPO, KWA, KWC, LUKA, NAB
C     !COM
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
      equivalence (IQQ(307),IQALO)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(26),KOMPO)
      equivalence (LUNITS(37),LUKA )
C     !DASH
C     !EJECT
      external AMAHO, HAOMA, HI, BYE
C
      call HI ('MAOHA')
C     !BEG
C---- These have to be done now (even though they may not be used)
C     because the values of KWC and KWA may be needed.
C
C---- Composite Line opacity
      call AMAHO (KOMPO, NAB, KWC)
C---- Averaged Line opacity
      call HAOMA (LUKA, IQALO, KWA)
C
C---- Statistical Line opacity is done later (in JIM via TRENT),
C     after absorber switches have been computed.
C
C     !END
      call BYE ('MAOHA')
C
      return
      end
