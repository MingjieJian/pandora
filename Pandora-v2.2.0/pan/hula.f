      subroutine HULA
     $(NO,JLV,N,M,RNU,RCP,XNU,XNUC,YW,TR,FA,FB,RKI,RKM,RLI,RLA,RLB,
     $ RLM,IRLS,CKI,DORK,DORL,KOOL,RKC,RKMC,RLC,RLMC,RLAC,RLBC,KSHL,
     $ PKS,TREFF,CP)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Writes out calculated rates for level JLV.
C     (This is version 3 of HULA.)
C     !DASH
      save
C     !DASH
      real*8 CKI, CP, FA, FB, PKS, RCP, RKC, RKI, RKM, RKMC, RLA, RLAC,
     $       RLB, RLBC, RLC, RLI, RLM, RLMC, RNU, TR, TREFF, XNU, XNUC,
     $       YW
      integer IB, IE, IQRID, IQRTA, IQTRP, IRLS, JLV, M, N, NO
      logical DETL, DORK, DORL, DORS, JTRF, KOOL, KSHL, PTRF, RALL
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
      equivalence (IQQ(291),IQTRP)
      equivalence (IQQ(180),IQRTA)
      equivalence (IQQ(108),IQRID)
C     !DASH
C     !EJECT
      external  NAUGHTD, UKULELE, SAMISEN, LINER, REBEC, HI, BYE
      intrinsic min
C
C               CP(NSL+1), RNU(MRX+1), FB(N,MRX+1), XNU(NSL), TREFF(N),
      dimension CP(*),     RNU(*),     FB(*),       XNU(*),   TREFF(*),
C
C               YW(N,MRX+1), TR(N,MRX+1), FA(N,MRX+1), PKS(N), RLBC(N),
     $          YW(*),       TR(*),       FA(*),       PKS(*), RLBC(*),
C
C               RCP(MRX+1), RKI(N), RKM(N), RLI(N), RLA(N), RLC(N,NSL),
     $          RCP(*),     RKI(*), RKM(*), RLI(*), RLA(*), RLC(*),
C
C               RLM(N), RKC(N), RKMC(N), XNUC(N), RLMC(NSL), RLAC(N),
     $          RLM(*), RKC(*), RKMC(*), XNUC(*), RLMC(*),   RLAC(*),
C
C               CKI(N), RLB(N)
     $          CKI(*), RLB(*)
C
      call HI ('HULA')
C     !BEG
      if(NO.gt.0) then
        call NAUGHTD (TREFF, 1, N, JTRF)
        PTRF = (.not.JTRF).and.(IQTRP.gt.0)
C
        DORS = (DORK.or.DORL).or.KSHL
        DETL = IQRID.gt.0
        RALL = IQRTA.gt.0
        call UKULELE   (NO, JLV, IRLS, DORS, DORK, KSHL, KOOL, PTRF,
     $                  DETL)
C
        IE = 0
  100   continue
          IB = IE+1
          IE = min(IE+8,N)
C
          call SAMISEN (NO, DORS, IB, IE, N, JLV, M, XNU, XNUC, RNU,
     $                  RCP, TR, YW, FA, FB, DETL, RALL, KSHL)
          call LINER   (1, NO)
          call REBEC   (NO, IB, IE, DORK, DORL, IRLS, KSHL, KOOL,
     $                  DETL, RKI, RKC, RLI, RLC, RKM, RKMC, RLA,
     $                  RLAC, RLB, RLBC, RLM, RLMC, CKI, PKS, TREFF,
     $                  PTRF, CP(JLV))
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('HULA')
C
      return
      end
