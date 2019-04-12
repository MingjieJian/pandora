      subroutine BRUTTO
     $(KDGV,KDFA,N,NL,GVI,GVL)
C
C     Rudolf Loeser, 1998 Jul 29
C---- Sets switches to reflect values of GVI and GVL.
C     !DASH
      save
C     !DASH
      real*8 GVI, GVL
      integer IQDFA, KDFA, KDGV, N, NL
      logical ZRI, ZRL
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
      equivalence (IQQ(234),IQDFA)
C     !DASH
      external NAUGHTD, HI, BYE
C
C               GVL(N,NL), GVI(N)
      dimension GVL(N,*),  GVI(*)
C
      call HI ('BRUTTO')
C     !BEG
      call NAUGHTD (GVL,1,(N*NL),ZRL)
      call NAUGHTD (GVI,1, N    ,ZRI)
      if((.not.ZRI).or.(.not.ZRL)) then
        KDGV = 1
        if((.not.ZRL).and.(IQDFA.gt.0)) then
          KDFA = 1
        end if
      end if
C     !END
      call BYE ('BRUTTO')
C
      return
      end
