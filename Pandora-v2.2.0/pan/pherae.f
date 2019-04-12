      subroutine PHERAE
     $(X,W,IW,N,VXS,GTN,PHP,COP,INDEX,WAVE,DL,DDL,FDDL,CDL,LDL,DP,DW,
     $ XNE,MPROM,VP,DV,AA,UU,XMU,EMU,LG,PHI,CKL,OPAC,WN,WH,Y,MOVING,
     $ ILFLX,TAUK,OPAW,TAUW,IMG,PHC,CKP,WRK,DUMP)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Gets comoving profile PHI, line opacity CKL, total opacity OPAC,
C     and weight matrices WN and WH, for all rays, in an expanding
C     plane-parallel atmosphere.
C     (See also PYDNA.)
C     (This is version 2 of PHERAE.)
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKP, COP, DDL, DL, DP, DV, DW, EMU, FDDL,
     $       GTN, OPAC, OPAW, PHC, PHI, PHP, TAUK, TAUW, UU, VP, VXS, W,
     $       WAVE, WH, WN, WRK, X, XMU, XNE, Y
      integer ILFLX, IMG, INDEX, IW, KODE, LDL, LG, M, MPROM, N
      logical DMPM, DUMP, MOVING
      character LABEL*100
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external SET1, PINDAR, YUKUM, ELCHO, TANAGRA, HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               WH(N,N,LG), CKL(N,N,LG), PHI(N,N,LG), FDDL(N), OPAW(N),
      dimension WH(N,N,*),  CKL(N,N,*),  PHI(N,N,*),  FDDL(*), OPAW(*),
C
C               XMU(LG), OPAC(N,N,LG), EMU(N), TAUK(N), VXS(N), XNE(N),
     $          XMU(*),  OPAC(N,N,*),  EMU(*), TAUK(*), VXS(*), XNE(*),
C
C               DP(N,LDL), DW(N), DDL(LDL), CDL(LDL), DV(N,LDL), DL(1),
     $          DP(*),     DW(*), DDL(*),   CDL(*),   DV(*),     DL(*),
C
C               COP(N), PHP(N), IMG(N), WN(N,N,LG), PHC(N,LDL), CKP(N),
     $          COP(*), PHP(*), IMG(*), WN(N,N,*),  PHC(*),     CKP(*),
C
C               VP(N), GTN(N), AA(N,LDL), UU(N,LDL), TAUW(N), WRK(N,N)
     $          VP(*), GTN(*), AA(*),     UU(*),     TAUW(*), WRK(*)
C     !EJECT
C
      call HI ('PHERAE')
C     !BEG
      do 101 M = 1,LG
C----   Set up dump, if needed
        call YUKUM   (M, LG, INDEX, 3, N, LABEL, DUMP, DMPM)
C----   Get comoving profile and line opacity
        call SET1    (EMU, N, XMU(M))
        call PINDAR  (N, N, N, WAVE, DL, EMU, VXS, DP, DW, XNE, MPROM,
     $                DDL, FDDL, CDL, LDL, GTN, VP, DV, AA, UU, PHP,
     $                PHC, CKP, PHI(1,1,M), CKL(1,1,M), W, IW, DMPM)
C----   Get total opacity
        call ELCHO   (N, N, N, CKL(1,1,M), COP, OPAC(1,1,M), WRK, DMPM)
C----   Get weight matrices
        call TANAGRA (X, W, IW, N, OPAC(1,1,M), LABEL, TAUK, XMU(M), Y,
     $                MOVING, WN(1,1,M), KODE, WH(1,1,M), ILFLX, IMG,
     $                M, OPAW, TAUW, WRK, DMPM)
        if(KODE.ne.1) then
          write (MSSLIN(1),100) KODE
  100     format('KODE =',I12,'; weight matrix calculation failed.')
          call HALT  ('PHERAE', 1)
        end if
  101 continue
C     !END
      call BYE ('PHERAE')
C
      return
      end
