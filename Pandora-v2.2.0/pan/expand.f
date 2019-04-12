      subroutine EXPAND
     $(KE,F,N,FE,NE)
C
C     Rudolf Loeser, 1971 Jan 04
C---- Expands the table F of length N by inserting  KE  equispaced
C     new values between each pair of consecutive values of F.
C     !DASH
      save
C     !DASH
      real*8 D, F, FE, ONE, Z
      integer I, J, KE, N, NE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               F(N), FE(NE)
      dimension F(*), FE(*)
C
      call HI ('EXPAND')
C     !BEG
      if(KE.gt.0) then
        D = KE+1
        D = ONE/D
C
        NE = 0
        do 101 I = 2,N
          Z = D*(F(I)-F(I-1))
C
          NE = NE+1
          FE(NE) = F(I-1)
          do 100 J = 1,KE
            NE = NE+1
            FE(NE) = FE(NE-1)+Z
  100     continue
  101   continue
C
        NE = NE+1
        FE(NE) = F(N)
      end if
C     !END
      call BYE ('EXPAND')
C
      return
      end
