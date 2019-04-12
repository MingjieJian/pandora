      subroutine BULLET
     $(X,W,IW,RZM,HND,XNE,ZHEL,ZME,AEL,HNK,Z,MODE)
C
C     Rudolf Loeser, 1980 Jul 29
C     Revised RL/SGK Apr  9 2014 
C---- Computes and prints analysis of relative electrons contributions.
C     (This is version 4 of BULLET.)
C     !DASH
      save
C     !DASH
      real*8 AEL, HND, HNK, RZM, W, X, XNE, Z, ZHEL, ZME
      integer IZTRM, IAMUX, IARRL, IETA, IETAS, IFMC, IFMCS, IFTC,
     $        IIMUX, IIPNT, IN, IQELO, IR, IRL, IS, IVEC, IW, IWS, IZL,
     $        IZMEL, JN, LU, MO, MODE, MOX, MUX, N, IZMER
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
      equivalence (IQQ( 67),IQELO)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external ZEUS, ICEMAN, PEARL, FIZMO, ELIOT, POCKET, OTAKAR, MONG,
     $         WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               RZM(N), HND(N), XNE(N), HNK(N), ZHEL(N), ZME(N), Z(N),
      dimension RZM(*), HND(*), XNE(*), HNK(*), ZHEL(*), ZME(*), Z(*),
C
C               AEL(N)
     $          AEL(*)
C
      dimension IN(13)
      equivalence
     $ (IN( 1),IETA  ),(IN( 2),IZL   ),(IN( 3),IFTC  ),(IN( 4),IFMC  ),
     $ (IN( 5),IETAS ),(IN( 6),IAMUX ),(IN( 7),IZTRM ),(IN( 8),IFMCS ),
     $ (IN( 9),IARRL ),(IN(10),IZMEL ),(IN(11),IR    ),(IN(12),IRL   ),
     $ (IN(13),IZMER )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IIMUX ),(JN( 2),IIPNT ),(JN( 3),IVEC  )
C
      call HI ('BULLET')
C     !BEG
      call ZEUS     (MO,IQELO,LU)
      if(LU.gt.0) then
C
C       (Get, and allocate, W & IW allotments)
        call ICEMAN (IN,IS ,MOX,'BULLET')
        call OTAKAR (JN,IWS,MUX,'BULLET')
C
C----   Compute ZME and ETA
        call PEARL  (X,W,XNE,ZME,W(IETA),W(IZTRM))
C----   Compute FMC and FTC
c
        call FIZMO  (N,X,XNE,W(IZTRM),W(IFTC),W(IFMC))
C----   Compute ZL and ZMER
        call ELIOT  (N,RZM,ZME,AEL,HND,W(IZMEL),W(IZMER))
C----   Print
        call MONG   (LU,N,ZME,RZM,AEL,HNK,XNE,ZHEL,W(IETA),W(IFMC),
     $               W(IFTC),W(IZMEL),W(IZMER),MODE)
C----   Plot
        call POCKET (LU,N,W(IFMC),W(IFMCS),W(IETA),W(IETAS),Z,W(IZL),
     $               W(IAMUX),IW(IIMUX),IW(IIPNT),IW(IVEC),W,W(IARRL),
     $               XNE,HNK,ZHEL,W(IZMEL),W(IR),W(IRL))
C
C       (Give back W & IW allotments)
        call WGIVE  (W ,'BULLET')
        call IGIVE  (IW,'BULLET')
      end if
C     !END
      call BYE ('BULLET')
C
      return
      end
