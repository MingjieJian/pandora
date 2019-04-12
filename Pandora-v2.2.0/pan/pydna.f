      subroutine PYDNA
     $(N,MRR,PHI,CKL,OPAC,WN,VXS,GTN,COP,WAVE,DL,DDL,FDDL,CDL,LDL,DP,
     $ DW,XNE,MPROM,FMULT,INDEX,TOPT,XRAY,EMU,VP,DV,AA,UU,PHP,TAUK,
     $ OPAW,TAUW,WH,Y,MOVING,ILFLX,PHC,CKP,IMG,WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Gets comoving profile PHI, line opacity CKL, total opacity OPAC,
C     and weight matrices WN and WH, for all disk rays, in an expanding
C     spherical atmosphere.
C     (See also PHERAE.)
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKP, COP, DDL, DL, DP, DV, DW, EMU, FDDL,
     $       FMULT, GTN, OPAC, OPAW, PHC, PHI, PHP, TAUK, TAUW, UU, VP,
     $       VXS, W, WAVE, WH, WN, WRK, XNE, XRAY, Y
      integer ILFLX, IMG, INDEX, IW, KODE, LDL, M, MPROM, MRR, N
      logical DMPM, DUMP, MOVING, TOPT
      character LABEL*100
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external PINDAR, YUKUM, ELCHO, BRANWEN, HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               OPAC(N,N,MRR), CKL(N,N,MRR), PHI(N,N,MRR), XRAY(N,MRR),
      dimension OPAC(N,N,*),   CKL(N,N,*),   PHI(N,N,*),   XRAY(N,*),
C
C               WN(N,N,MRR), EMU(N,MRR), GTN(N), XNE(N), VXS(N), DL(1),
     $          WN(N,N,*),   EMU(N,*),   GTN(*), XNE(*), VXS(*), DL(*),
C
C               WH(N,N,MRR), FDDL(N), DV(N,LDL), AA(N,LDL), PHC(N,LDL),
     $          WH(N,N,*),   FDDL(*), DV(*),     AA(*),     PHC(*),
C
C               DP(N,LDL), DW(N), TAUK(N,N), CDL(LDL), DDL(LDL), VP(N),
     $          DP(*),     DW(*), TAUK(*),   CDL(*),   DDL(*),   VP(*),
C
C               UU(N,LDL), PHP(N), OPAW(N), TAUW(N), CKP(N), WRK(N,N),
     $          UU(*),     PHP(*), OPAW(*), TAUW(*), CKP(*), WRK(*),
C
C               IMG(N), COP(N)
     $          IMG(*), COP(*)
C     !EJECT
C
      call HI ('PYDNA')
C     !BEG
      do 101 M = 1,MRR
C----   Set up dump, if needed
        call YUKUM   (M, MRR, INDEX, 2, N, LABEL, DUMP, DMPM)
C----   Get comoving profile and line opacity
        call PINDAR  (N, N, N, WAVE, DL, EMU(1,M), VXS, DP, DW, XNE,
     $                MPROM, DDL, FDDL, CDL, LDL, GTN, VP, DV, AA, UU,
     $                PHP, PHC, CKP, PHI(1,1,M), CKL(1,1,M), W, IW,
     $                DMPM)
C----   Get total opacity
        call ELCHO   (N, N, N, CKL(1,1,M), COP, OPAC(1,1,M), WRK, DMPM)
C----   Get weight matrices
        call BRANWEN (N, XRAY(1,M), OPAC(1,1,M), LABEL, TAUK, TOPT,
     $                WN(1,1,M), KODE, M, OPAW, TAUW, WH(1,1,M), Y,
     $                MOVING, ILFLX, IMG, WRK, W, IW, DMPM)
        if(KODE.ne.1) then
          write (MSSLIN(1),100) KODE
  100     format('KODE =',I12,'; weight matrix calculation failed.')
          call HALT  ('PYDNA',1)
        end if
  101 continue
C     !END
      call BYE ('PYDNA')
C
      return
      end
