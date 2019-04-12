      subroutine CAW
     $(M,DEL,F,FN,G,R,A,B,C,D)
C
C     Rudolf Loeser, 1997 Aug 27
C---- Computes "original" A, B, C, D, for 4-diagonal solution.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DEL, DLM, DLP, F, FN, G, HALF, ONE, R, TRM,
     $       ZERO
      integer I, M
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
      external HI, BYE
C
C               DEL(N), F(N), FN(N), G(N), R(N), A(N), B(N), C(N), D(N)
      dimension DEL(*), F(*), FN(*), G(*), R(*), A(*), B(*), C(*), D(*)
C     !EJECT
C
      call HI ('CAW')
C     !BEG
      A(1) = ZERO
      D(1) = ZERO
C
      B(1) = R(1)
      C(1) = ZERO
C----
      DLP  =  DEL(3)+DEL(2)
      TRM  =  (F(2)-F(1))/DEL(2)
C
      A(2) = -FN(2)/(DEL(3)*DLP)
      D(2) =  ZERO
C
      B(2) =  G(2)/DEL(2)+HALF*R(2)-(TRM-FN(2)/DEL(3))/DEL(2)
      C(2) = -G(1)/DEL(2)+HALF*R(1)+(TRM-FN(2)/DLP   )/DEL(2)
C----
      do 100 I = 3,(M-1)
        DLM  =  DEL(I-1)+DEL(I)
        DLP  =  DEL(I+1)+DEL(I)
        TRM  =  (F(I)-F(I-1))/DEL(I)
C
        A(I) = -FN(I)/(DEL(I+1)*DLP)
        D(I) = -FN(I)/(DEL(I-1)*DLM)
C
        B(I) =  G(I  )/DEL(I)+HALF*R(I  )-
     $          (FN(I)*(ONE/DLM-ONE/DEL(I+1))+TRM)/DEL(I)
        C(I) = -G(I-1)/DEL(I)+HALF*R(I-1)-
     $          (FN(I)*(ONE/DLP-ONE/DEL(I-1))-TRM)/DEL(I)
  100 continue
C----
      DLM  =  DEL(M-1)+DEL(M)
      TRM  =  (F(M)-F(M-1))/DEL(M)
C
      A(M) =  ZERO
      D(M) = -FN(M)/(DEL(M-1)*DLM)
C
      B(M) =  G(M  )/DEL(M)+HALF*R(M  )-(FN(M)/DLM     +TRM)/DEL(M)
      C(M) = -G(M-1)/DEL(M)+HALF*R(M-1)+(FN(M)/DEL(M-1)+TRM)/DEL(M)
C     !END
      call BYE ('CAW')
C
      return
      end
