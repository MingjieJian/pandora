      subroutine ISOTRIA
     $(N,FRS,S,SR,PSR,XJBAR,AW,EP,BS,CHI,ICE,GMMA,XR,PGM,PDR)
C
C     Rudolf Loeser, 2004 May 07
C---- Sets up stuff to be printed by KALAN.
C     (This is version 3 of ISOTRIA.)
C     !DASH
      save
C     !DASH
      real*8 AW, BS, CHI, EP, FRS, GMMA, ONE, S, SR, XJBAR, XR, ZERO
      integer I, ICE, IQSFS, N
      logical PDR, PGM, PSR
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
      equivalence (IQQ( 31),IQSFS)
C     !DASH
C     !EJECT
      external CARLY, OXUS, HI, BYE
C
C               XJBAR(N), FRS(N), SR(N), AW(N), EP(N), BS(N), CHI(N),
      dimension XJBAR(*), FRS(*), SR(*), AW(*), EP(*), BS(*), CHI(*),
C
C               S(N)
     $          S(*)
C
      call HI ('ISOTRIA')
C     !BEG
      do 100 I = 1,N
        call CARLY (EP(I), AW(I), XJBAR(I), BS(I), CHI(I))
  100 continue
C
      PSR = IQSFS.gt.0
      if(PSR) then
        call OXUS  (S, FRS, SR, N)
      end if
C
      PGM = (GMMA.lt.ZERO).and.(ICE.ne.0)
      PDR = (XR.eq.(-ONE)).and.(ICE.ne.0)
C     !END
      call BYE ('ISOTRIA')
C
      return
      end
