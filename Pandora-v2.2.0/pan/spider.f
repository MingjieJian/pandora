      subroutine SPIDER
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Drives production of Contribution Summaries.
C     (This is version 2 of SPIDER.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IIMG, IN, INDX, INXW, IPER, IQABS, IQEMS, IQTAS, IS, IW,
     $        IWS, IWTAB, IWTBW, IWVNUM, IX, IXCBL, JN, KODE, LEGEND,
     $        MF, ML, MOX, MUX
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
      equivalence (IQQ( 59),IQABS)
      equivalence (IQQ(104),IQEMS)
      equivalence (IQQ(105),IQTAS)
C     !DASH
C     !EJECT
      external IAGO, IRKEDA, BURMA, IGIVE, WALERAN, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IWTAB ),(IN( 2),IWTBW),(IN( 3),IWVNUM ),(IN( 4),IXCBL )
C
      dimension JN(4)
      equivalence
     $(JN( 1),INDX  ),(JN( 2),INXW  ),(JN( 3),IPER  ),(JN( 4),IIMG  )
C
      call HI ('SPIDER')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call IAGO      (IN, IS,  MOX, 'SPIDER')
      call WALERAN   (JN, IWS, MUX, 'SPIDER')
C
C---- Set up basic data, and get table selection indices MF and ML
      call IRKEDA    (W(IWVNUM), W(IWTAB), IW(INDX), MF, ML, KODE)
C
      if(KODE.eq.1) then
        LEGEND = 0
        if(IQABS.gt.0) then
C----     Absorbers
          call BURMA (X, W, 1, LEGEND, W(IXCBL), W(IWTAB), IW(INDX),
     $                MF, ML, IW(IIMG), W(IWTBW), IW(INXW), IW(IPER))
        end if
        if(IQEMS.gt.0) then
C----     Emitters
          call BURMA (X, W, 2, LEGEND, W(IXCBL), W(IWTAB), IW(INDX),
     $                MF, ML, IW(IIMG), W(IWTBW), IW(INXW), IW(IPER))
        end if
        if(IQTAS.gt.0) then
C----     Optical depths
          call BURMA (X, W, 3, LEGEND, W(IXCBL), W(IWTAB), IW(INDX),
     $                MF, ML, IW(IIMG), W(IWTBW), IW(INXW), IW(IPER))
        end if
      end if
C
C     (Give back W & IW allotments)
      call WGIVE     (W,  'SPIDER')
      call IGIVE     (IW, 'SPIDER')
C     !END
      call BYE ('SPIDER')
C
      return
      end
