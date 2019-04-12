      subroutine OLEOSA
     $(GHI,GXI,XNK,GVI,GHL,GXL,XND,GVL,GVL1,N,MN1,NL,DUMP)
C
C     Rudolf Loeser, 1987 Dec 07
C---- Computes total diffusion terms GVI and GVL for TARPON.
C     !DASH
      save
C     !DASH
      real*8 GHI, GHL, GVI, GVL, GVL1, GXI, GXL, XND, XNK
      integer IQAN1, LUEO, MN1, N, NL, NNL
      logical DUMP
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
      equivalence (IQQ(272),IQAN1)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  ARRADD, ARRDIV, MOVE1, STOAT, MASHED, MESHED, HI, BYE
C
C               GVL1(N), GXI(N), XNK(N), GVI(N), GHL(N,NL), GXL(N,NL),
      dimension GVL1(*), GXI(*), XNK(*), GVI(*), GHL(N,*),  GXL(N,*),
C
C               XND(N,NL), GVL(N,NL), GHI(N)
     $          XND(N,*),  GVL(N,*),  GHI(*)
C     !EJECT
C
      call HI ('OLEOSA')
C     !BEG
C---- Ion term
      call ARRADD   (GHI, GXI, GVI, N)
      call ARRDIV   (GVI, XNK, GVI, N)
C---- Levels terms
      NNL = N*NL
      call ARRADD   (GHL, GXL, GVL, NNL)
      call ARRDIV   (GVL, XND, GVL, NNL)
C
      if((IQAN1.gt.0).and.(MN1.gt.0)) then
C----   Save level-1 values for G-CHECK
        call MOVE1  (GVL(1,1), MN1, GVL1)
      end if
C
      if(DUMP) then
        call MESHED ('OLEOSA', 2)
        call STOAT  (LUEO, N, NL, GVL, GVI, 'unsmoothed')
        call MASHED ('OLEOSA')
      end if
C     !END
      call BYE ('OLEOSA')
C
      return
      end
