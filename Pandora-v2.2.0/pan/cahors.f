      subroutine CAHORS
     $(X,W,IW,N,VXS,GTN,COP,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,XMU,WAVE,
     $ DL,LL,LG,IND,PHI,Y,MOVING,WN,RKODE,WH,ILFLX,PHP,PHC,CKP,VP,DV,
     $ AA,UU,EMU,CKL,OPAC,TMU,OPAW,TAUW,IMG,WRK,DUMP)
C
C     Rudolf Loeser, 1983 Mar 01
C---- Sets up comoving profile, PHI, weight matrices, WN and WH,
C     and TAU codes, RKODE, for all rays,
C     expanding plane-parallel atmosphere.
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKP, COP, DDL, DL, DP, DV, DW, EMU, FDDL,
     $       GTN, OPAC, OPAW, PHC, PHI, PHP, RKODE, TAUW, TMU, UU, VP,
     $       VXS, W, WAVE, WH, WN, WRK, X, XMU, XNE, Y
      integer ILFLX, IMG, IND, IW, KODE, LDL, LG, LL, MM, MPROM, N
      logical DMPM, DUMP, MOVING
      character LABEL*100
C     !DASH
      external ULFIN, SET1, PINDAR, ELCHO, TANAGRA, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               PHI(N,N,LG), WN(N,N,LG), RKODE(LG), DP(N,LDL), FDDL(N),
      dimension PHI(N,N,*),  WN(N,N,*),  RKODE(*),  DP(*),     FDDL(*),
C
C               DDL(LDL), DL(1), VXS(N), DW(N), XNE(N), EMU(N), PHP(N),
     $          DDL(*),   DL(1), VXS(*), DW(*), XNE(*), EMU(*), PHP(*),
C
C               CDL(LDL), GTN(N), VP(N), DV(N,LDL), WH(N,N,LG), IMG(N),
     $          CDL(*),   GTN(*), VP(*), DV(*),     WH(N,N,*),  IMG(*),
C
C               AA(N,LDL), UU(N,LDL), CKP(N), XMU(LG), OPAW(N), COP(N),
     $          AA(*),     UU(*),     CKP(*), XMU(*),  OPAW(*), COP(*),
C
C               PHC(N,LDL), OPAC(N,N), CKL(N,N), TMU(N,N), WRK(N,N),
     $          PHC(*),     OPAC(*),   CKL(*),   TMU(*),   WRK(*),
C
C               TAUW(N)
     $          TAUW(*)
C     !EJECT
C
      call HI ('CAHORS')
C     !BEG
      do 100 MM = 1,LG
        IND = IND+1
C----   Set up dump, if needed
        call ULFIN   (MM, LG, LL, IND, 3, N, LABEL, DUMP, DMPM)
C----   Get comoving profile and line opacity
        call SET1    (EMU, N, XMU(MM))
        call PINDAR  (N, N, N, WAVE, DL, EMU, VXS, DP, DW, XNE, MPROM,
     $                DDL, FDDL, CDL, LDL, GTN, VP, DV, AA, UU, PHP,
     $                PHC, CKP, PHI(1,1,MM), CKL, W, IW, DMPM)
C----   Get total opacity
        call ELCHO   (N, N, N, CKL, COP, OPAC, WRK, DMPM)
C----   Get weight matrices and codes
        call TANAGRA (X, W, IW, N, OPAC, LABEL, TMU, XMU(MM), Y,
     $                MOVING, WN(1,1,MM), KODE, WH(1,1,MM), ILFLX, IMG,
     $                MM, OPAW, TAUW, WRK, DMPM)
        RKODE(MM) = KODE
  100 continue
C     !END
      call BYE ('CAHORS')
C
      return
      end
