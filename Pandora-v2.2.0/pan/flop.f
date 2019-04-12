      subroutine FLOP
     $(N,MRR,R1N,Z,FRR,NRAD,RADI,DPTH)
C
C     Rudolf Loeser, 1982 Apr 09
C---- Makes RADI and DPTH, for FLURRY.
C     !DASH
      save
C     !DASH
      real*8 D, DPTH, FRR, ONE, R1N, RADI, Z
      integer I, J, MRR, MRRM, N, NM, NRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               Z(N), FRR(MRR), RADI(NRAD), DPTH(NRAD)
      dimension Z(N), FRR(*),   RADI(*),    DPTH(*)
C
      call HI ('FLOP')
C     !BEG
      MRRM = max((MRR-1),0)
      NM   = N-1
      NRAD = NM+MRRM
C
      J = NRAD+1
      D = R1N+Z(N)
      do 100 I = 2,N
        J = J-1
        RADI(J) = (D-Z(I))/R1N
        DPTH(J) = Z(I)
  100 continue
C
      if(MRRM.gt.0) then
        D = Z(N)
        do 101 I = 1,MRRM
          RADI(I) = FRR(I)
          DPTH(I) = D+(ONE-FRR(I))*R1N
  101   continue
      end if
C     !END
      call BYE ('FLOP')
C
      return
      end
