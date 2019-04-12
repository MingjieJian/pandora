      subroutine EUSAPIA
     $(K,A,LDL,CDL,N,SAP,XJNU,PHC,XJBR)
C
C     Rudolf Loeser, 2005 Mar 31
C---- Computes JBAR by integrating JNU, for PRD.
C     (This is version 6 of EUSAPIA.)
C     !DASH
      save
C     !DASH
      real*8 A, CDL, FAC, HALF, PHC, SAP, SUMJB, V, XJBR, XJNU, ZERO
      integer I, J, K, L, LDL, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external ZERO1, DIVIDE, HI, BYE
C
C               PHC(N,K,LDL), SAP(N,LDL), XJNU(N,K), CDL(LDL), XJBR(N),
      dimension PHC(N,K,*),   SAP(N,*),   XJNU(N,*), CDL(*),   XJBR(*),
C
C               A(K)
     $          A(*)
C
      call HI ('EUSAPIA')
C     !BEG
      call ZERO1      (XJBR, N)
      do 102 L = 1,LDL
        do 101 I = 1,N
          call DIVIDE (CDL(L), SAP(I,L), FAC)
          SUMJB = ZERO
          do 100 J = 1,K
            V = HALF*A(J)
            SUMJB = SUMJB+(V*PHC(I,J,L))*XJNU(I,J)
  100     continue
          XJBR(I) = XJBR(I)+FAC*SUMJB
  101   continue
  102 continue
C     !END
      call BYE ('EUSAPIA')
C
      return
      end
