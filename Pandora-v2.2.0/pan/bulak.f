      subroutine BULAK
     $(N,NL,RKI,IQRK,RLI,IQRL,LUP,CXX,CXXP,NPQ,LCX,ESG,POPK,POPN,LIMP)
C
C     Rudolf Loeser, 1990 Nov 27
C---- Applies upper level charge exchange effect to RKI and RLI
C     of "XED"-ions.
C     LIMP .ge. NPQNX is required!
C     !DASH
      save
C     !DASH
      real*8 CXX, CXXP, ESG, POPK, POPN, RAT, RKI, RLI
      integer I, IQRK, IQRL, J, LCX, LIMP, LUP, N, NL, NPQ, PQN
C     !DASH
      external MULSUM, DIVIDE, KALANK, HI, BYE
C
C               RKI(N,NSL), RLI(N,NSL), CXX(N,NL), CXXP(N,NL), NPQ(NL),
      dimension RKI(N,*),   RLI(N,*),   CXX(N,*),  CXXP(N,*),  NPQ(*),
C
C               IQRK(NSL), IQRL(NSL), ESG(N,NL), POPK(N), POPN(N,LIMP),
     $          IQRK(*),   IQRL(*),   ESG(N,*),  POPK(*), POPN(N,*),
C
C               LCX(NL)
     $          LCX(*)
C
      call HI ('BULAK')
C     !BEG
      do 101 J = 1,NL
        PQN = NPQ(J)
        if(LCX(J).gt.0) then
C
          if(IQRK(J).gt.0) then
            call MULSUM   (CXX(1,J),POPK,RKI(1,J),N)
          end if
C
          if(IQRL(J).gt.0) then
            do 100 I = 1,N
              call DIVIDE (CXXP(I,J),ESG(I,J),RAT)
              RLI(I,J) = RLI(I,J)+RAT*POPN(I,PQN)
  100       continue
          end if
C
        end if
  101 continue
C---- Print
      call KALANK         (N,NL,LUP,RKI,IQRK,RLI,IQRL,ESG)
C     !END
      call BYE ('BULAK')
C
      return
      end
