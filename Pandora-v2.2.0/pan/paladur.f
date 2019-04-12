      subroutine PALADUR
     $(N,VXS,GTN,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,LDL,XRAY,Z,R1N,WAVE,
     $ DL,LL,NSHL,NRPMX,CODSRW,IND,TOPT,PHI,WN,WH,ILFLX,Y,MOVING,RKODE,
     $ OPARAY,CKLRAY,PHIRAY,DPX,DWX,VXX,XNEX,CPX,EMX,GTX,FDLX,PHX,VP,
     $ DV,AA,UU,TNU,TAUW,OPAW,WNR,WHR,PHC,CKP,IMG,WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Gets PHI, computed-WN, and RKODE, for Shell rays.
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKLRAY, CKP, CODSRW, COP, CPX, DDL, DL, DP, DPX,
     $       DV, DW, DWX, EMX, FDDL, FDLX, GTN, GTX, OPARAY, OPAW, PHC,
     $       PHI, PHIRAY, PHX, R1N, RKODE, TAUW, TNU, UU, VP, VXS, VXX,
     $       W, WAVE, WH, WHR, WN, WNR, WRK, XNE, XNEX, XRAY, Y, Z
      integer I, IDM, ILFLX, IMG, IMX, IND, IW, KODE, KODSRW, LDL, LL,
     $        MM, MPROM, N, NRP, NRPMX, NSHL
      logical DMPM, DUMP, MOVING, TOPT
      character LABEL*100
C     !DASH
      external TATAR, BARBARA, KIRGIZ, PINDAR, DEIRA, ULFIN, BARABAS,
     $         ARAWN, ELCHO, HI, BYE
C
      dimension W(*), IW(*)
C
C               PHI(N,N,NSHL), WN(N,N,NSHL), RKODE(NSHL), Z(N), XNE(N),
      dimension PHI(N,N,*),    WN(N,N,*),    RKODE(*),    Z(*), XNE(*),
C
C               OPARAY((N+3),NRPMX), CKLRAY((N+3),NRPMX), XNEX(NRPMX),
     $          OPARAY(*),           CKLRAY(*),           XNEX(*),
C
C               PHIRAY((N+3),NRPMX), XRAY(NRPMX,NSHL), DW(N), DDL(LDL),
     $          PHIRAY(*),           XRAY(NRPMX,*),    DW(*), DDL(*),
C
C               WH(N,N,NSHL), VXS(N), GTN(N), COP(N), WHR(NRPMX,NRPMX),
     $          WH(N,N,*),    VXS(*), GTN(*), COP(*), WHR(*),
C
C               TNU(NRPMX,NRPMX), DWX(NRPMX), VXX(NRPMX), CODSRW(NSHL),
     $          TNU(*),           DWX(*),     VXX(*),     CODSRW(*),
C
C               CPX(NRPMX), DP(N,LDL), EMX(NRPMX), GTX(NRPMX), FDDL(N),
     $          CPX(*),     DP(*),     EMX(*),     GTX(*),     FDDL(*),
C
C               PHX(NRPMX), TAUW(NRPMX), DV(NRPMX,LDL), PHC(NRPMX,LDL),
     $          PHX(*),     TAUW(*),     DV(*),         PHC(*),
C
C               UU(NRPMX,LDL), CKP(NRPMX), FDLX(NRPMX), DPX(NRPMX,LDL),
     $          UU(*),         CKP(*),     FDLX(*),     DPX(*),
C
C               OPAW(NRPMX), WNR(NRPMX,NRPMX), CDL(LDL), AA(NRPMX,LDL),
     $          OPAW(*),     WNR(*),           CDL(*),   AA(*),
C
C               WRK(NRPMX,NRPMX), VP(NRPMX), DL(1), IMG(N)
     $          WRK(*),           VP(*),     DL(*), IMG(*)
C     !EJECT
C
      call HI ('PALADUR')
C     !BEG
      IDM = N+3
      I   = 0
      do 100 MM = 1,NSHL
        IND = IND+1
        call TATAR   (I)
        NRP = 2*I+5
        IMX = I+3
C----   Set up dump, if needed
        call ULFIN   (MM, NSHL, LL, IND, 1, I, LABEL, DUMP, DMPM)
C----   Extend data along ray
        call BARABAS (I, DP, N, DPX, NRP, LDL)
        call BARBARA (DW  , I, DWX )
        call BARBARA (VXS , I, VXX )
        call BARBARA (COP , I, CPX )
        call BARBARA (GTN , I, GTX )
        call BARBARA (XNE , I, XNEX)
        call BARBARA (FDDL, I, FDLX)
C----   Get comoving profile and line opacity
        call KIRGIZ  (R1N, Z(N), Z, I, EMX)
        call PINDAR  (IMX, IDM, NRP, WAVE, DL, EMX, VXX, DPX, DWX,
     $                XNEX, MPROM, DDL, FDLX, CDL, LDL, GTX, VP, DV,
     $                AA, UU, PHX, PHC, CKP, PHIRAY, CKLRAY, W, IW,
     $                DMPM)
        call DEIRA   (PHIRAY, PHI(1,1,MM), NRPMX, I, NRP, IMX, IDM,
     $                TAUW)
C----   Get total opacity
        call ELCHO   (IMX, IDM, NRP, CKLRAY, CPX, OPARAY, WRK, DMPM)
        KODSRW = CODSRW(MM)
        if(KODSRW.eq.0) then
C----     Get weight matrices and KODE
          call ARAWN (N, I, IMX, IDM, NRP, XRAY(1,MM), OPARAY, LABEL,
     $                TOPT, MM, WN(1,1,MM), KODE, TNU, TAUW, OPAW, WNR,
     $                WH(1,1,MM), WHR, Y, MOVING, ILFLX, IMG, WRK,
     $                W, IW, DMPM)
        else
C----     Get KODE only
          KODE = 1
        end if
        RKODE(MM) = KODE
  100 continue
C     !END
      call BYE ('PALADUR')
C
      return
      end
