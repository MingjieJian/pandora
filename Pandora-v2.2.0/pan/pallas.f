      subroutine PALLAS
     $(N,NMX,INC,GAW,DMP1,DMP2,IND,A,PHI,PHIW,C,GTN,XKPC,BC,
     $ XJNU,SIG,T1,T2,T3,WN,TNU,EXT,ANT,PA,PG,PB,OMD,DLC,PQ,PD,AW)
C
C     Rudolf Loeser, 1981 Dec 03
C           revised, 2004 May 03
C
C---- Core of summation over frequencies and directions,
C     for Line Source Functions calculations.
C     !DASH
      save
C     !DASH
      real*8 A, ANT, AW, BC, C, CAP, CDWW, COD, CODW, DEM, DEMW, DLC,
     $       EXT, GTN, OMD, PA, PB, PD, PG, PHI, PHIW, PKJ, PKJW, PQ,
     $       SIG, T1, T2, T3, TNU, WN, XJNU, XKB, XKL, XKLW, XKPC
      integer I, IND, J, N, NMX
      logical DMP1, DMP2, GAW, INC
C     !DASH
      external WINE, MAAS, MEMEL, DIVIDE, HI, BYE
C
C               PHI(N), C(N), PB(NMX), XKPC(N), SIG(N), BC(N), AW(NMX),
      dimension PHI(*), C(*), PB(*),   XKPC(*), SIG(*), BC(*), AW(*),
C
C               GTN(N), XJNU(N), WN(N,N), OMD(NMX), PHIW(N,N), PD(NMX),
     $          GTN(*), XJNU(*), WN(N,*), OMD(*),   PHIW(N,*), PD(*),
C
C               T1(N), T2(N), T3(N), PA(NMX,NMX), PG(NMX,NMX), PQ(NMX),
     $          T1(*), T2(*), T3(*), PA(NMX,*),   PG(NMX,*),   PQ(*),
C
C               ANT(NMX), DLC(NMX), TNU(N), EXT(N)
     $          ANT(*),   DLC(*),   TNU(*), EXT(*)
C     !EJECT
C
      call HI ('PALLAS')
C     !BEG
      call WINE        (DMP1, DMP2, IND, N, WN)
C
      do 101 I = 1,N
        CAP    = C(I)*A*PHI(I)
        ANT(I) = ANT(I)+CAP
C
        if(INC) then
          PD(I) = PD(I)+CAP*EXT(I)
        end if
        if(GAW) then
          AW(I) = AW(I)+CAP*WN(I,I)
        end if
C
        XKL = GTN(I)*PHI(I)
        XKB = XKPC(I)+SIG(I)
        DEM = XKL+XKB
        call DIVIDE    (CAP, DEM, COD)
        PKJ    = (XKPC(I)*BC(I))+(SIG(I)*XJNU(I))+(XKL*T3(I))
C
        OMD(I) = OMD(I)+COD*XKL*T1(I)
        PQ(I)  = PQ(I) +COD*(XKB+XKL*T2(I))
        DLC(I) = DLC(I)+COD*PKJ
C
        if(DMP2) then
          call MAAS    (IND, I, PHI(I), XKL, DEM, PKJ, BC(I), TNU(I),
     $                  DLC(I), OMD(I), PQ(I), PD(I), GTN(I), C(I), A,
     $                  XKPC(I), SIG(I), XJNU(I), T1(I), T2(I), T3(I),
     $                  CAP, ANT(I), WN(I,I), AW(I))
        end if
C
        do 100 J = 1,N
          XKLW = GTN(J)*PHIW(I,J)
          DEMW = XKLW+XKPC(J)+SIG(J)
          call DIVIDE  (CAP, DEMW, CODW)
          CDWW = CODW*WN(I,J)
          PKJW = (XKPC(J)*BC(J))+(SIG(J)*XJNU(J))+(XKLW*T3(J))
C
          PA(I,J) = PA(I,J)+CDWW*XKLW*T1(J)
          PG(I,J) = PG(I,J)+CDWW*PKJW
          PB(I)   = PB(I)  +CDWW*PKJW
C
          if(DMP2) then
            call MEMEL (IND, I, J, DEMW, XKLW, PKJW, WN(I,J), PHIW(I,J),
     $                  PA(I,J), PG(I,J), PB(I))
          end if
C
  100   continue
  101 continue
C     !END
      call BYE ('PALLAS')
C
      return
      end
