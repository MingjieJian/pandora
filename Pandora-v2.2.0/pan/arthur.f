      subroutine ARTHUR
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Controls final net rates calculations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer ICOOL, IGD, IHEAT, IICOL, IIHET, IN, IQCCR, IQCHR, IS,
     $        IVEC, IW, IX, MOX, N
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
      equivalence (IQQ(133),IQCCR)
      equivalence (IQQ(231),IQCHR)
C     !DASH
      external MABON, HUMBLE, DYLAN, TIGRIS, ZERO1, WGIVE, GUSHER, FOSS,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ICOOL ),(IN( 2),IICOL ),(IN( 3),IHEAT ),(IN( 4),IIHET ),
     $(IN( 5),IGD   ),(IN( 6),IVEC  )
C     !EJECT
C
      call HI ('ARTHUR')
C     !BEG
C     (Get, and allocate, W allotment)
      call MABON    (IN,IS,MOX,'ARTHUR')
C
      call ZERO1    (W(ICOOL),N)
      call ZERO1    (W(IICOL),N)
      call ZERO1    (W(IHEAT),N)
      call ZERO1    (W(IIHET),N)
C---- Get gas density
      call GUSHER   (X,N,W(IGD),W(IVEC))
      if(IQCCR.gt.0) then
C----   Cooling
        call HUMBLE (X,W,IW,W(IGD),W(ICOOL),W(IICOL))
      end if
      if(IQCHR.gt.0) then
C----   Heating
        call DYLAN  (X,W,W(IGD),W(IHEAT),W(IIHET))
      end if
      if((IQCCR.gt.0).and.(IQCHR.gt.0)) then
C----   Combined printout and graphs
        call TIGRIS (X,W,W(ICOOL),W(IICOL),W(IHEAT),W(IIHET))
      end if
C---- Debug checksums
      call FOSS     (W(ICOOL),W(IICOL),W(IHEAT),W(IIHET))
C
C     (Give back W allotment)
      call WGIVE    (W,'ARTHUR')
C     !END
      call BYE ('ARTHUR')
C
      return
      end
