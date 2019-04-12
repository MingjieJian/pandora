      subroutine TARAN
     $(JLEV,N,MRP,KSHL,KOOL,DORK,DORL,IRLS,CP,RCP,RNU,YW,RJ,UPJ,RKM,
     $ PKS,RKI,RKMC,XNU,RKC,UJ,FA,FB,RLA,RLB,RLM,RLI,RLAC,RLBC,RLMC,RLC)
C
C     Rudolf Loeser, 1984 Jan 20
C---- Computes rates for level JLEV, for MANU.
C     !DASH
      save
C     !DASH
      real*8 CP, CPJ, FA, FB, PKS, RCP, RJ, RKC, RKI, RKM, RKMC, RLA,
     $       RLAC, RLB, RLBC, RLC, RLI, RLM, RLMC, RNU, UJ, UPJ, XNU,
     $       XNUJ, YW
      integer I, IQRID, IRLS, JLEV, MRP, N
      logical DETL, DORK, DORL, KOOL, KSHL, MOOH
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      equivalence (IQQ(108),IQRID)
C     !DASH
      external FAME, GAME, HALT, HI, BYE
C
C               RLAC(N), RCP(MRX+1), RLB(N), RLMC(N), XNU(NSL), RLA(N),
      dimension RLAC(*), RCP(*),     RLB(*), RLMC(*), XNU(*),   RLA(*),
C
C               CP(NSL+1), RKM(N), PKS(N), RKI(N), FB(N,MRX+1), UPJ(N),
     $          CP(*),     RKM(*), PKS(*), RKI(*), FB(*),       UPJ(*),
C
C               RLC(N), RNU(MRX+1), YW(N,MRX+1), UJ(N), RLI(N), RLM(N),
     $          RLC(*), RNU(*),     YW(*),       UJ(*), RLI(*), RLM(*),
C
C               RKC(N), RLBC(N), FA(N,MRX+1), RKMC(N)
     $          RKC(*), RLBC(*), FA(*),       RKMC(*)
C     !EJECT
C
      call HI ('TARAN')
C     !BEG
      if((IRLS.lt.1).or.(IRLS.gt.2)) then
        write (MSSLIN(1),100) IRLS
  100   format('IRLS =',I12,', which is neither 1 nor 2.')
        call HALT     ('TARAN', 1)
      end if
C
      DETL = IQRID.gt.0
      MOOH = .false.
C
      CPJ  = CP(JLEV)
      XNUJ = XNU(JLEV)
C---- Loop over all depths
      do 101 I = 1,N
C
        if(KSHL) then
C----     Get PKS
          call FAME   (CPJ, RCP, RNU, MRP, YW, RJ, UPJ, I, N, MOOH,
     $                 XNUJ, RKM(I), PKS(I))
        else
C
C----     Get RK
          if(DORK) then
            call FAME (CPJ, RCP, RNU, MRP, YW, RJ, UPJ, I, N, MOOH,
     $                 XNUJ, RKM(I), RKI(I))
          end if
          if(KOOL) then
            call FAME (CPJ, RCP, RNU, MRP, YW, RJ, UPJ, I, N, KOOL,
     $                 XNUJ, RKMC(I), RKC(I))
          end if
C
C----     Get RL
          if(DORL) then
            call GAME (CPJ, RCP, RNU, MRP, YW, RJ, UJ, I, N, DETL,
     $                 IRLS, MOOH, XNUJ, FA, FB, RLA(I), RLB(I),
     $                 RLM(I), RLI(I))
          end if
          if(KOOL) then
            call GAME (CPJ, RCP, RNU, MRP, YW, RJ, UJ, I, N, DETL,
     $                 IRLS, KOOL, XNUJ, FA, FB, RLAC(I), RLBC(I),
     $                 RLMC(I), RLC(I))
          end if
        end if
C
  101 continue
C     !END
      call BYE ('TARAN')
C
      return
      end
