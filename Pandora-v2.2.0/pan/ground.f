      subroutine GROUND
     $(TAUIJ,TS,W,IW)
C
C     Rudolf Loeser, 1980 May 02
C---- Controls PICTURE, for printing collated TAU scales.
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, TS, W
      integer ICTAU, IIPNT, IIVEC, IMARK, IN, IQSCL, IS, IW, IWS, JN,
     $        LU, M, MO, MOX, MUX, N, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 3),M  )
      equivalence (JZQ( 5),NT )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ(  2),IQSCL)
C     !DASH
C     !EJECT
      external ZAMA, ZEUS, KILT, IGIVE, PICTURE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               TAUIJ(N,NT), TS(M)
      dimension TAUIJ(*),    TS(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),ICTAU )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IMARK ),(JN( 2),IIPNT ),(JN( 3),IIVEC )
C
      call HI ('GROUND')
C     !BEG
      call ZEUS      (MO,IQSCL,LU)
      if(LU.gt.0) then
C       (Get, and allocate, W & IW allotments)
        call ZAMA    (IN,IS ,MOX,'GROUND')
        call KILT    (JN,IWS,MUX,'GROUND')
C
        call PICTURE (LU,TS,M,TAUIJ,N,NT,W(ICTAU),IW(IMARK),IW(IIPNT),
     $                IW(IIVEC))
C
C       (Give back W & IW allotments)
        call WGIVE   (W ,'GROUND')
        call IGIVE   (IW,'GROUND')
      end if
C     !END
      call BYE ('GROUND')
C
      return
      end
