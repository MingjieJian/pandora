      subroutine MANU
     $(LUP,LUG,LUS,NSL,LIM,N,MRJ,RRNUIJ,RRCPIJ,RNU,RCP,XNU,XNUC,YW,FA,
     $ FB,TR,IQRK,IQRL,RKI,RKM,RLI,RLM,RLA,RLB,CP,TE,YRATE,YR,RNUP,
     $ RCPP,TRS,YWS,UJ,UPJ,XCBL,IRLS1,IRLSN,CKI,JOOL,RKC,RKMC,RLC,RLMC,
     $ RLAC,RLBC,TREFF,PKS,WT,W)
C
C     Rudolf Loeser, 1984 Jan 20
C---- Computes RKI and RLI, and related Cooling Terms, from Jnu.
C     (This is version 4 of MANU.)
C     !DASH
      save
C     !DASH
      real*8 CKI, CP, FA, FB, PKS, RCP, RCPP, RJ, RKC, RKI, RKM, RKMC,
     $       RLA, RLAC, RLB, RLBC, RLC, RLI, RLM, RLMC, RNU, RNUP,
     $       RRCPIJ, RRNUIJ, TE, TR, TREFF, TRS, UJ, UPJ, W, WT, XCBL,
     $       XNU, XNUC, YR, YRATE, YW, YWS
      integer IQRK, IQRL, IRLS, IRLS1, IRLSN, J, LIM, LUG, LUP, LUS, MR,
     $        MRJ, MRP, MRPP, N, NSL, NWT
      logical DOJN, DORK, DORL, JOOL, KOOL, KSHL
C     !DASH
      external VOOM, HULA, KYNAN, PAPAYA, BEEFUP, BEDWYR, TARAN, PLUTO,
     $         UBANGI, LENTIL, HARP, HI, BYE
C
      dimension W(*)
C
C     LIM = NSL + min(KSHL,1)
C
C               YWS(N,LIM), TRS(N,LIM), RKI(N,NSL), RLI(N,NSL), PKS(N),
      dimension YWS(N,*),   TRS(N,*),   RKI(N,*),   RLI(N,*),   PKS(*),
C
C               RRNUIJ(MRS), RRCPIJ(MRS), RLC(N,NSL), RKM(N), XNU(NSL),
     $          RRNUIJ(*),   RRCPIJ(*),   RLC(N,*),   RKM(*), XNU(*),
C
C               MRJ(NSL+1), RNU(MRX+1), CKI(N,NSL), RKC(N,NSL), RLM(N),
     $          MRJ(*),     RNU(*),     CKI(N,*),   RKC(N,*),   RLM(*),
C
C               RCP(MRX+1), YW(N,MRX+1), FA(N,MRX+1), RLAC(N), RLBC(N),
     $          RCP(*),     YW(*),       FA(*),       RLAC(*), RLBC(*),
C
C               FB(N,MRX+1), TR(N,MRX+1), TREFF(N,LIM), RLA(N), RLB(N),
     $          FB(*),       TR(*),       TREFF(N,*),   RLA(*), RLB(*),
C
C               CP(NSL+1), TE(N), YRATE(MRS), RNUP(MRX+1), RCPP(MRX+1),
     $          CP(*),     TE(*), YRATE(*),   RNUP(*),     RCPP(*),
C
C               YR(MMR+1), IQRK(NSL), UPJ(N), XCBL(Miklen), WT(Numkon),
     $          YR(*),     IQRK(*),   UPJ(*), XCBL(*),      WT(*),
C
C               RKMC(N), RLMC(N), UJ(N), IQRL(NSL), XNUC(NSL)
     $          RKMC(*), RLMC(*), UJ(*), IQRL(*),   XNUC(*)
C     !EJECT
C
      call HI ('MANU')
C     !BEG
      IRLS = IRLS1
C---- Make edited Wavelengths table
      call UBANGI     (WT, NWT)
C---- Loop over all levels
      do 100 J = 1,LIM
C----   Set control switches
        call LENTIL   (J, NSL, IQRK, IQRL, KOOL, JOOL, KSHL, DORK,
     $                 DORL, DOJN)
        if(DOJN) then
C----     Get basic integration input tables of "RNU" and "RCP",
C           (call them RNUP, RCPP, of length MRPP).
          call PAPAYA (J, RNUP, RCPP, YR, MR, MRPP, MRJ, RRNUIJ,
     $                 RRCPIJ, YRATE)
C----     Get augmented tables of "RNU" and "RCP",
C           (call them RNU and RCP, of length MRP, where MRP .ge. MRPP).
          call BEEFUP (RNUP, RCPP, MRPP, RNU, RCP, MRP, J, XNU, XNUC,
     $                 WT, NWT)
C----     Get Jnu and TR for each RNU of this level
          call PLUTO  (XCBL, MRP, RNU, RCP, DOJN, YW, TR, N, XNU,
     $                 XNUC, J, YWS(1,J), TRS(1,J), LUS)
C----     Other initialization amd intermediates
          call BEDWYR (RLI(1,J), N, MRP, KSHL, RKM, RLM, FA, FB,
     $                 RLA, RLB)
          call VOOM   (YW, XNU, MRP, N, TE, RNU, J, NSL, RJ, UJ, UPJ,
     $                 KSHL, 1)
C----     Now compute Rates by integration
C           (Note that MRP is the number of integration points).
          call TARAN  (J, N, MRP, KSHL, KOOL, DORK, DORL, IRLS, CP,
     $                 RCP, RNU, YW, RJ, UPJ, RKM, PKS, RKI(1,J),
     $                 RKMC, XNU, RKC(1,J), UJ, FA, FB, RLA, RLB, RLM,
     $                 RLI(1,J), RLAC, RLBC, RLMC, RLC(1,J))
C----     Compute effective radiation temperature
          call KYNAN  (J, N, MRP, CP, XNU, RKI(1,J), TR, KSHL, DORK,
     $                 TREFF(1,J), W)
        else
          MRP = 1
C           (and YWS(1,J), TRS(1,J) and TREFF(1,J) will remain =0!)
        end if
C----   Print and plot
        call HULA     (LUP, J, N, MRP, RNU, RCP, XNU, XNUC, YW, TR,
     $                 FA, FB, RKI(1,J), RKM, RLI(1,J), RLA, RLB, RLM,
     $                 IRLS, CKI(1,J), DORK, DORL, KOOL, RKC(1,J),
     $                 RKMC, RLC(1,J), RLMC, RLAC, RLBC, KSHL, PKS,
     $                 TREFF(1,J), CP)
        call HARP     (LUG, J, DOJN, KSHL, TE, YW, N, RNU, MRP, XNU,
     $                XNUC)
C----
        IRLS = IRLSN
  100 continue
C     !END
      call BYE ('MANU')
C
      return
      end
