      subroutine PECAN
     $(N,NL,M,PKL,PL1,PLK,P1L,GMI,CKI,XND,ARHO,CIJ,CL1TA,C1LTA,RKI,
     $ CQSI,GVL,KDGV,DUMP)
C
C     Rudolf Loeser, 1976 Nov 26
C---- Computes and prints intermediates, for COCOS.
C     (This is version 2 of PECAN.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, C1LTA, CIJ, CKI, CL1TA, CQSI, GMI, GVL, P1L, PKL,
     $       PL1, PLK, RKI, TERM, XND, ZERO
      integer I, KDGV, L, LM, LS, M, ML, N, NL
      logical DUMP, GTERM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, MOVE1, INDXIJ, FILBERT, DIVIDE, HI, BYE
C
C               GVL(N,NL), ARHO(N,NL), GMI(N,NSL), P1L(N), C1LTA(N,NL),
      dimension GVL(N,*),  ARHO(N,*),  GMI(N,*),   P1L(*), C1LTA(N,*),
C
C               CQSI(N,NSL), RKI(N,NSL), CKI(N,NSL), PKL(N), XND(N,NL),
     $          CQSI(N,*),   RKI(N,*),   CKI(N,*),   PKL(*), XND(N,*),
C
C               PL1(N), PLK(N), CL1TA(N,NL), CIJ(N,NL**2)
     $          PL1(*), PLK(*), CL1TA(N,*),  CIJ(N,*)
C     !EJECT
C
      call HI ('PECAN')
C     !BEG
      LS = 0
      GTERM = (KDGV.gt.0)
C---- Initialize P-terms
      call ZERO1       (PL1, N)
      call ZERO1       (P1L, N)
      call ZERO1       (PLK, N)
      call ZERO1       (PKL, N)
C
C---- Compute P-terms
      do 101 L = 2,NL
C
        do 100 I = 1,N
C
          call INDXIJ  (M, L, ML)
          call INDXIJ  (L, M, LM)
          call FILBERT (LS, L, I, GMI(I,L), CQSI(I,L), RKI(I,L),
     $                  CKI(I,L), XND(I,L), ARHO(I,L), CIJ(I,LM),
     $                  CL1TA(I,L), CIJ(I,ML), C1LTA(I,L), DUMP)
C
          P1L(I) = P1L(I)+(CIJ(I,ML)+C1LTA(I,L))
C
          PL1(I) = PL1(I)+XND(I,L)*(ARHO(I,L)+(CIJ(I,LM)+CL1TA(I,L)))
C
          call DIVIDE  (CQSI(I,L), GMI(I,M), TERM)
          PKL(I) = PKL(I)+TERM
C
          if(GTERM) then
            TERM = GVL(I,L)
          else
            TERM = ZERO
          end if
          PLK(I) = PLK(I)+XND(I,L)*(RKI(I,L)+CKI(I,L)+TERM)
C
  100   continue
C
  101 continue
C     !END
      call BYE ('PECAN')
C
      return
      end
