      subroutine PALLENE
     $(N,NSHL,NRPMX,CODSRW,PHI,CKL,OPAC,WN,VXS,GTN,COP,WAVE,DL,DP,DW,
     $ XNE,MPROM,DDL,FDDL,CDL,LDL,FMULT,INDEX,TOPT,XRAY,Z,R1N,OPARAY,
     $ CKLRAY,PHIRAY,DPX,DWX,XNEX,FDDLX,VXX,CPX,EMX,GTX,PHX,VP,DV,AA,
     $ UU,TAUK,TAUW,OPAW,WNR,WH,WHR,Y,MOVING,ILFLX,PHC,CKP,IMG,WRK,
     $ W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Feb 24
C---- Gets PHI, CKL, OPAC, and weight matrices, for shell rays.
C     (This is version 2 of PALLENE.)
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKLRAY, CKP, CODSRW, COP, CPX, DDL, DL, DP,
     $       DPX, DV, DW, DWX, EMX, FDDL, FDDLX, FMULT, GTN, GTX, OPAC,
     $       OPARAY, OPAW, PHC, PHI, PHIRAY, PHX, R1N, TAUK, TAUW, UU,
     $       VP, VXS, VXX, W, WAVE, WH, WHR, WN, WNR, WRK, XNE, XNEX,
     $       XRAY, Y, Z
      integer I, IDM, ILFLX, IMG, IMX, INDEX, IW, KODE, KODSRW, LDL, MM,
     $        MPROM, N, NRP, NRPMX, NSHL
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
C     !EJECT
      external TATAR, BARBARA, KIRGIZ, PINDAR, DEIRA, BARABAS, ELCHO,
     $         ARAWN, YUKUM, HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               OPAC(N,N,NSHL), CKL(N,N,NSHL), PHI(N,N,NSHL), DDL(LDL),
      dimension OPAC(N,N,*),    CKL(N,N,*),    PHI(N,N,*),    DDL(*),
C
C               WN(N,N,NSHL), XRAY(NRPMX,NSHL), Z(N), GTN(N), CDL(LDL),
     $          WN(N,N,*),    XRAY(NRPMX,*),    Z(*), GTN(*), CDL(*),
C
C               OPARAY((N+3),NRPMX), CKLRAY((N+3),NRPMX), CODSRW(NSHL),
     $          OPARAY(*),           CKLRAY(*),           CODSRW(*),
C
C               DP(N,LDL), VP(NRPMX), XNEX(NRPMX), PHIRAY((N+3),NRPMX),
     $          DP(*),     VP(*),     XNEX(*),     PHIRAY(*),
C
C               OPAW(NRPMX), DW(N), VXS(N), COP(N), CKP(NRPMX), IMG(N),
     $          OPAW(*),     DW(*), VXS(*), COP(*), CKP(*),     IMG(*),
C
C               DPX(NRPMX,LDL), DWX(NRPMX), VXX(NRPMX), PHC(NRPMX,LDL),
     $          DPX(*),         DWX(*),     VXX(*),     PHC(*),
C
C               EMX(NRPMX), GTX(NRPMX), PHX(NRPMX), XNE(N), CPX(NRPMX),
     $          EMX(*),     GTX(*),     PHX(*),     XNE(*), CPX(*),
C
C               FDDL(N), AA(NRPMX,LDL), WHR(NRPMX,NRPMX), WH(N,N,NSHL),
     $          FDDL(*), AA(*),         WHR(*),           WH(N,N,*),
C
C               TAUW(NRPMX), UU(NRPMX,LDL), TAUK(NRPMX,NRPMX), DL(1),
     $          TAUW(*),     UU(*),         TAUK(*),           DL(*),
C
C               WNR(NRPMX,NRPMX), DV(NRPMX,LDL), WRK(NRPMX,NRPMX),
     $          WNR(*),           DV(*),         WRK(*),
C
C               FDDLX(NRPMX)
     $          FDDLX(*)
C     !EJECT
C
      call HI ('PALLENE')
C     !BEG
      IDM = N+3
      I   = 0
      do 101 MM = 1,NSHL
        call TATAR    (I)
        NRP = 2*I+5
        IMX = I+3
C----   Set up dump, if needed
        call YUKUM    (MM, NSHL, INDEX, 1, I, LABEL, DUMP, DMPM)
C----   Extend data along ray
        call BARABAS  (I, DP, N, DPX, NRP, LDL)
        call BARBARA  (DW  , I, DWX  )
        call BARBARA  (VXS , I, VXX  )
        call BARBARA  (COP , I, CPX  )
        call BARBARA  (GTN , I, GTX  )
        call BARBARA  (XNE , I, XNEX )
        call BARBARA  (FDDL, I, FDDLX)
C----   Get comoving profile and line opacity
        call KIRGIZ   (R1N, Z(N), Z, I, EMX)
        call PINDAR   (IMX, IDM, NRP, WAVE, DL, EMX, VXX, DPX, DWX,
     $                 XNEX, MPROM, DDL, FDDLX, CDL, LDL, GTX, VP, DV,
     $                 AA, UU, PHX, PHC, CKP, PHIRAY, CKLRAY, W, IW,
     $                 DMPM)
        call DEIRA    (PHIRAY, PHI(1,1,MM), NRPMX, I, NRP, IMX, IDM,
     $                 TAUW)
        call DEIRA    (CKLRAY, CKL(1,1,MM), NRPMX, I, NRP, IMX, IDM,
     $                 TAUW)
C----   Get total opacity
        call ELCHO    (IMX, IDM, NRP, CKLRAY, CPX, OPARAY, WRK, DMPM)
        call DEIRA    (OPARAY, OPAC(1,1,MM), NRPMX, I, NRP, IMX, IDM,
     $                 TAUW)
        KODSRW = CODSRW(MM)
        if(KODSRW.eq.0) then
C----     Get weight matrices
          call ARAWN  (N, I, IMX, IDM, NRP, XRAY(1,MM), OPARAY, LABEL,
     $                 TOPT, MM, WN(1,1,MM), KODE, TAUK, TAUW, OPAW,
     $                 WNR, WH(1,1,MM), WHR, Y, MOVING, ILFLX, IMG,
     $                 WRK, W, IW, DMPM)
          if(KODE.ne.1) then
            write (MSSLIN(1),100) KODE
  100       format('KODE =',I12,'; optical depth calculations ',
     $             '(for weight matrix) failed.')
            call HALT ('PALLENE', 1)
          end if
        end if
  101 continue
C     !END
      call BYE ('PALLENE')
C
      return
      end
