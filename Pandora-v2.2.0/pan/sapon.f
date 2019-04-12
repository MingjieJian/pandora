      subroutine SAPON
     $(N,K,LDL,A,PHC,SAP)
C
C     Rudolf Loeser, 1978 Apr 22
C---- Computes SAP, a PRD term.
C     !DASH
      save
C     !DASH
      real*8 A, HALF, PHC, SAP
      integer I, J, K, L, LDL, N, NLDL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external ZERO1, CONMUL, HI, BYE
C
C               A(K), PHC(N,K,LDL), SAP(N,LDL)
      dimension A(*), PHC(N,K,*),   SAP(N,*)
C
      call HI ('SAPON')
C     !BEG
      NLDL = N*LDL
      call ZERO1  (SAP,NLDL)
C
      do 102 L = 1,LDL
        do 101 J = 1,K
          do 100 I = 1,N
            SAP(I,L) = SAP(I,L)+A(J)*PHC(I,J,L)
  100     continue
  101   continue
  102 continue
C
      call CONMUL (HALF,SAP,NLDL)
C     !END
      call BYE ('SAPON')
C
      return
      end
