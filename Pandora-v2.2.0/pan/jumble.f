      subroutine JUMBLE
     $(WN,IXUSE,NE,NZ,KK,XJIK,SLY,XLB,SP,CNXP,XJBN)
C
C     Rudolf Loeser, 1974 Dec 23
C---- Computes XJIK, for ROPE.
C
C     (Note: NZ equals N; NE (i.e. "n to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, SLY, SP, WN, XJBN, XJIK, XLB
      integer IQJLY, IS, IXUSE, K, KK, NE, NZ
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
      equivalence (IQQ(215),IQJLY)
C     !DASH
C     !EJECT
      external FUMBLE, MOVE1, DUMBLE, NUMBLE, HI, BYE
C
C               WN(NZ,NZ,KKX), XJIK(NZ,KKX), CNXP(NZ,KKX), XLB(NZ,KKX),
      dimension WN(NZ,NZ,*),   XJIK(NZ,*),   CNXP(*),      XLB(NZ,*),
C
C               SLY(NZ,KKX), XJBN(NZ,KKX), IXUSE(KKX), SP(NZ,KKX)
     $          SLY(NZ,*),   XJBN(*),      IXUSE(*),   SP(NZ,*)
C
      call HI ('JUMBLE')
C     !BEG
      if(NE.gt.0) then
C----   Compute XJIK, I .le. NE
        do 100 K = 1,KK
          if(IXUSE(K).eq.1) then
            call DUMBLE (NE, XLB(1,K), SLY(1,K), SP(1,K), WN(1,1,K),
     $                   XJIK(1,K))
          end if
  100   continue
      end if
C
      IS = NE+1
      if(IS.le.NZ) then
C----   Compute XJIK, I .gt. NE
        call NUMBLE     (IS, NZ, KK, XLB, SLY, SP, XJIK)
      end if
C
C---- Add Incident Radiation term, if needed
      call FUMBLE       (NZ, KK, CNXP, XJIK)
C
      if(IQJLY.gt.0) then
C----   Save
        call MOVE1      (XJIK, (NZ*KK), XJBN)
      end if
C     !END
      call BYE ('JUMBLE')
C
      return
      end
