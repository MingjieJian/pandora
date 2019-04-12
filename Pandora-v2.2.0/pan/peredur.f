      subroutine PEREDUR
     $(N,VXS,GTN,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,LDL,EMU,XRAY,WAVE,
     $ DL,LL,MRR,IND,TOPT,PHI,WN,RKODE,PHP,VP,DV,AA,UU,CKL,OPAC,TNU,
     $ OPAW,TAUW,WH,ILFLX,Y,MOVING,PHC,CKP,IMG,WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Gets PHI, WN, WH and RKODE, for all disk rays, RAGAN.
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKP, COP, DDL, DL, DP, DV, DW, EMU, FDDL,
     $       GTN, OPAC, OPAW, PHC, PHI, PHP, RKODE, TAUW, TNU, UU, VP,
     $       VXS, W, WAVE, WH, WN, WRK, XNE, XRAY, Y
      integer ILFLX, IMG, IND, IW, KODE, LDL, LL, MM, MPROM, MRR, N
      logical DMPM, DUMP, MOVING, TOPT
      character LABEL*100
C     !DASH
      external PINDAR, ULFIN, ELCHO, BRANWEN, HI, BYE
C
      dimension W(*), IW(*)
C
C               PHI(N,N,MRR), CDL(LDL), XRAY(N,MRR), EMU(N,MRR), DL(1),
      dimension PHI(N,N,*),   CDL(*),   XRAY(N,*),   EMU(N,*),   DL(*),
C
C               RKODE(MRR), WH(N,N,MRR), VXS(N), GTN(N), COP(N), VP(N),
     $          RKODE(*),   WH(N,N,*),   VXS(*), GTN(*), COP(*), VP(*),
C
C               DP(N,LDL), IMG(N), DDL(LDL), PHP(N), FDDL(N), TNU(N,N),
     $          DP(*),     IMG(*), DDL(*),   PHP(*), FDDL(*), TNU(*),
C
C               DV(N,LDL), AA(N,LDL), UU(N,LDL), OPAC(N,N), PHC(N,LDL),
     $          DV(*),     AA(*),     UU(*),     OPAC(*),   PHC(*),
C
C               OPAW(N), XNE(N), CKL(N,N), TAUW(N), WN(N,N,MRR), DW(N),
     $          OPAW(*), XNE(*), CKL(*),   TAUW(*), WN(N,N,*),   DW(*),
C
C               WRK(N,N), CKP(N)
     $          WRK(*),   CKP(*)
C     !EJECT
C
      call HI ('PEREDUR')
C     !BEG
      do 100 MM = 1,MRR
        IND = IND+1
C----   Set up dump, if needed
        call ULFIN   (MM, MRR, LL, IND, 2, N, LABEL, DUMP, DMPM)
C----   Get comoving profile and line opacity
        call PINDAR  (N, N, N, WAVE, DL, EMU(1,MM), VXS, DP, DW, XNE,
     $                MPROM, DDL, FDDL, CDL, LDL, GTN, VP, DV, AA, UU,
     $                PHP, PHC, CKP, PHI(1,1,MM), CKL, W, IW, DMPM)
C----   Get total opacity
        call ELCHO   (N, N, N, CKL, COP, OPAC, WRK, DMPM)
C----   Get weight matrices
        call BRANWEN (N, XRAY(1,MM), OPAC, LABEL, TNU, TOPT,
     $                WN(1,1,MM), KODE, MM, OPAW, TAUW, WH(1,1,MM), Y,
     $                MOVING, ILFLX, IMG, WRK, W, IW, DMPM)
        RKODE(MM) = KODE
  100 continue
C     !END
      call BYE ('PEREDUR')
C
      return
      end
