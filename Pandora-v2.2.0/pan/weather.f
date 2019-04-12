      subroutine WEATHER
     $(Z,NW,WTAB,XINT,WS,SNU,LEGEND,KODE,LU,IW)
C
C     Rudolf Loeser, 1973 Dec 05
C---- Drives depth-of-formation printing.
C
C     KODE = 1 means: for continuum; = 2 means: for line profile.
C     !DASH
      save
C     !DASH
      real*8 SNU, WS, WTAB, XINT, Z
      integer IIWS, IQORI, IQOSH, ISIG, IW, IWS, JN, KODE, KOELS, LU,
     $        MF, ML, MUX, N, NW
      logical LEGEND, SHORT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KZQ(180),KOELS)
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
      equivalence (IQQ(101),IQORI)
      equivalence (IQQ(190),IQOSH)
C     !DASH
C     !EJECT
      external MISTY, OOBLECK, BOGUMIL, IGIVE, HI, BYE
C
      dimension IW(*)
C
C               MM = KM or Nmkuse
C
C               WTAB(MM), XINT(MM), WS(N,MM), SNU(N,MM), Z(N)
      dimension WTAB(*),  XINT(*),  WS(*),    SNU(*),    Z(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),ISIG  ),(JN( 2),IIWS  )
C
      call HI ('WEATHER')
C     !BEG
      if((IQORI.gt.0).and.(LU.gt.0)) then
C
        call BOGUMIL   (WTAB,NW,MF,ML)
C
        if((ML-MF).ge.0) then
C         (Get, and allocate, IW allotment)
          call MISTY   (JN,IWS,MUX,'WEATHER',NW)
C
          SHORT = IQOSH.gt.0
          call OOBLECK (LU,SHORT,N,MF,ML,WTAB,XINT,WS,SNU,Z,IW(IIWS),
     $                  IW(ISIG),LEGEND,KODE,KOELS)
C
C         (Give back IW allotment)
          call IGIVE   (IW,'WEATHER')
        end if
C
      end if
C     !END
      call BYE ('WEATHER')
C
      return
      end
