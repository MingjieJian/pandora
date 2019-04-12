      subroutine JAFFA
     $(N,K,A,DL,DW,DP,TAUM,GMA,PHC,PHI,XJNU,SAP,NDR,XDR,DDR,XC,XP,
     $ DRLIMI,LDL,DDL,FDDL,CDL,FAB,FJN,FJJ,XK1,DJB)
C
C     Rudolf Loeser, 1978 Apr 22
C---- Computes the PRD terms: FAB, FJN, FJJ, DJB and K1.
C
C     These calculations are combined here because they share (all
C     except DJB) the calculation of DR in a quadruple loop.
C     (See also GAFFE.)
C     !DASH
      save
C     !DASH
      real*8 A, CDL, DDL, DDR, DIF, DJB, DL, DOR, DP, DR, DRLIMI, DW,
     $       FAB, FAC, FDDL, FJJ, FJN, GMA, HALF, ONE, PHC, PHI, PHRAT,
     $       SAP, SUMAB, SUMDJ, SUMJJ, SUMJN, SUMK1, TAUM, W, XC, XDR,
     $       XJNU, XK1, XP, ZERO, dummy
      integer I, II, J, JJ, JP, K, L, LDL, N, NDR, NK
      logical DMPI, DMPJ, DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZIEGEL, DRINK, ZERO1, DIVIDE, BROCKEN, OSBERT, MASHED,
     $         DRAT, HI, BYE
C
C               PHC(N,K,LDL), TAUM(N), XJNU(N,K), SAP(N,LDL), DDR(NDR),
      dimension PHC(N,K,*),   TAUM(*), XJNU(N,*), SAP(N,*),   DDR(*),
C
C               FJN(N,K), FJJ(N,K), DDL(LDL), A(K), PHI(N,K), XDR(NDR),
     $          FJN(N,*), FJJ(N,*), DDL(*),   A(*), PHI(N,*), XDR(*),
C
C               DL(K), CDL(LDL), DP(N,LDL), DRLIMI(N), FAB(N,K), DW(N),
     $          DL(*), CDL(*),   DP(*),     DRLIMI(*), FAB(N,*), DW(*),
C
C               GMA(N), XK1(N,K), FDDL(N), DJB(N,K)
     $          GMA(*), XK1(N,*), FDDL(*), DJB(N,*)
C
      call HI ('JAFFA')
C     !BEG
      NK = N*K
      call ZERO1   (FAB, NK)
      call ZERO1   (FJN, NK)
      call ZERO1   (FJJ, NK)
      call ZERO1   (XK1, NK)
      call ZERO1   (DJB, NK)
      call BROCKEN (N, K, 'JAFFA', DUMP, II, JJ)
C     !EJECT
      do 103 L = 1,LDL
        do 102 I = 1,N
          DMPI = DUMP.and.(I.eq.II)
          call ZIEGEL     (DMPI, L, CDL(L), I, SAP(I,L))
C
          call DIVIDE     (CDL(L), SAP(I,L), FAC)
          do 101 J = 1,K
            DMPJ = DMPI.and.(J.eq.JJ)
            call DIVIDE   (PHC(I,J,L), PHI(I,J), PHRAT)
C
            SUMAB = ZERO
            SUMJN = ZERO
            SUMJJ = ZERO
            SUMK1 = ZERO
            SUMDJ = ZERO
            do 100 JP = 1,K
              call DRINK  (I, N, JP, J, L, DL, DDL, FDDL, DW, DP, TAUM,
     $                     NDR, XDR, DDR, XC, XP, DRLIMI, DMPJ, DR)
C
              W   = HALF*A(JP)*PHC(I,JP,L)
              DOR = ONE-(PHRAT*DR)
C
              call OSBERT (A(JP), PHC(I,JP,L), GMA(I), XJNU(I,JP),
     $                     PHRAT, DOR, DMPJ)
C
              SUMAB = SUMAB+W*(           DOR)
              SUMJN = SUMJN+W*(           DOR)*XJNU(I,JP)
              SUMJJ = SUMJJ+W*(ONE-GMA(I)*DOR)*XJNU(I,JP)
C
              call DRAT   (XJNU(I,J), XJNU(I,JP), DIF, dummy)
              SUMK1 = SUMK1+DOR*W*DIF
              SUMDJ = SUMDJ+    W*DIF
C
  100       continue
            FAB(I,J) = FAB(I,J)+FAC*SUMAB
            FJN(I,J) = FJN(I,J)+FAC*SUMJN
            FJJ(I,J) = FJJ(I,J)+FAC*SUMJJ
            XK1(I,J) = XK1(I,J)+FAC*SUMK1
            DJB(I,J) = DJB(I,J)+FAC*SUMDJ
C
  101     continue
  102   continue
  103 continue
C
      if(DUMP) then
        call MASHED       ('JAFFA')
      end if
C     !END
      call BYE ('JAFFA')
C
      return
      end
