      subroutine MELINDA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Sep 05
C---- Allocates scratch storage for QUIXOTE.
C     (This is version 3 of MELINDA.)
C     !DASH
      save
C     !DASH
      integer IN, IQEXA, IS, LEN, MUX
      character CALLER*(*)
C     !COM
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
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
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MELINDA')
C     !BEG
      call WGET (IS,  CALLER)
C
      if(IQEXA.le.0) then
        LEN = LPDLEN
      else
        LEN = LODLEN
      end if
C
      IN( 1) = IS
C
      MUX    = IN( 1)+LEN
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MELINDA')
C
      return
      end
