      subroutine FLIP
     $(X,IX,W,IW,KRJ,RHOIJ,RHOO,XJBAR,BDIJ,IFUDGE,KODE,LEGEND,FLIPP,IMG)
C
C     Rudolf Loeser, 1981 Feb 13.
C---- Controls FLIBBLE, to compute basic B-ratios.
C
C     KRJ=1 for using RHOIJ (and RHOO) and JBAR;
C     KRJ=2 for using only JBAR;
C     KRJ=3 for using only RHOIJ (and RHOO).
C
C     KODE=1 if RHO-fudging data should be saved; =0 if not.
C     (This is version 3 of FLIP.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, FLIPP, RHOIJ, RHOO, W, X, XJBAR
      integer IFO, IFUDGE, IMG, IN, IQBRF, IRHNW, IS, IW, IWEIT, IX,
     $        IXM, IXMS, IXR, IZ, JFUDGE, JJCIJ, JJGM, JJKIJ, JJPIJ,
     $        JJXND, KODE, KRJ, MOX, N, NL, NSL, NT
      character LEGEND*33
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(144),JJPIJ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !EJECT
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
      equivalence (IQQ(106),IQBRF)
C     !DASH
      external IRK, ZEUS, FLIBBLE, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RHOIJ(N,NT), RHOO(N,NT), XJBAR(N,NT), BDIJ(N,NL), IMG(N)
      dimension RHOIJ(*),    RHOO(*),    XJBAR(*),    BDIJ(*),    IMG(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IXR   ),(IN( 2),IZ    ),(IN( 3),IXM   ),(IN( 4),IXMS  ),
     $(IN( 5),IRHNW ),(IN( 6),IWEIT ),(IN( 7),IFO   )
C     !EJECT
C
      call HI ('FLIP')
C     !BEG
      if(NL.gt.1) then
C
C       (Get, and allocate, W allotment)
        call IRK     (IN,IS,MOX,'FLIP')
C
        if((KRJ.eq.1).or.(KRJ.eq.3)) then
          call ZEUS  (IFUDGE,IQBRF,JFUDGE)
        else
          JFUDGE = 0
        end if
C
        call FLIBBLE (X,IX,W,IW,N,NL,NSL,NT,KRJ,X(JJGM),X(JJXND),BDIJ,
     $                RHOIJ,IX(JJKIJ),RHOO,W(IRHNW),W(IWEIT),XJBAR,
     $                KODE,FLIPP,JFUDGE,W(IXR),W(IXM),W(IXMS),W(IZ),
     $                IMG,W(IFO),X(JJCIJ),X(JJPIJ),LEGEND)
C
C       (Give back W allotment)
        call WGIVE   (W,'FLIP')
      end if
C     !END
      call BYE ('FLIP')
C
      return
      end
