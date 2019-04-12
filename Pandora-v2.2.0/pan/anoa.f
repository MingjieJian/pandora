      subroutine ANOA
     $(LIM,FXI,DV,RV,X,H,F)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Computes sample table of intermediates H and F, for "Sobolev"
C     escape probability integration, to be exhibited in a dump.
C     !DASH
      save
C     !DASH
      real*8 DV, EIGHTH, F, FXI, H, RV, VEC, X, ZERO
      integer I, J, LIM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(18),EIGHTH)
C     !DASH
      external PACA, HI, BYE
C
C               FXI(LIM), DV(LIM), RV(LIM), X(9), H(LIM,9), F(LIM,9)
      dimension FXI(*),   DV(*),   RV(*),   X(*), H(LIM,*), F(LIM,*)
C
      dimension VEC(4)
C
      call HI ('ANOA')
C     !BEG
      X(1) = ZERO
      do 100 I = 2,9
        X(I) = X(I-1)+EIGHTH
  100 continue
C
      do 102 I = 1,LIM
C
        VEC(1) = FXI(I)
        VEC(2) = DV(I)
        VEC(3) = RV(I)
C
        do 101 J = 1,9
          call PACA (VEC,X(J),F(I,J))
          H(I,J) = VEC(4)
  101   continue
C
  102 continue
C     !END
      call BYE ('ANOA')
C
      return
      end
