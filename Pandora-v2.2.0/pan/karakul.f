      subroutine KARAKUL
     $(WVL,DDL,FDDL,DL,K,DP,DW,V,N,MUSE,DV,A,U,PHI)
C
C     Rudolf Loeser, 1981 Apr 24
C---- Computes absorption profile (Voigt function) values, and
C     also values of DV, A and U.
C     !DASH
      save
C     !DASH
      real*8 A, DDL, DL, DP, DV, DW, FDDL, ONE, PHI, R, U, V, WVL, ZERO
      integer I, J, K, MUSE, N
      logical UZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  EPSOM, DIVIDE, MUTTON, HI, BYE
      intrinsic abs
C
C               PHI(N,K), U(N,K), DL(K), DP(K), DW(N), FDDL(N), DV(N),
      dimension PHI(N,*), U(N,*), DL(*), DP(*), DW(*), FDDL(*), DV(*),
C
C               A(N), V(N)
     $          A(*), V(*)
C
      call HI ('KARAKUL')
C     !BEG
      UZERO = MUSE.eq.0
C
      call EPSOM      (WVL, V, N, DV)
C
      do 101 I = 1,N
        call DIVIDE   (ONE, DW(I), R)
        A(I) = R*DP(I)
        do 100 J = 1,K
          if(UZERO) then
            U(I,J) = ZERO
          else
            U(I,J) = R*abs(DL(J)-(DDL*FDDL(I))+DV(I))
          end if
          call MUTTON (U(I,J), A(I), PHI(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('KARAKUL')
C
      return
      end
