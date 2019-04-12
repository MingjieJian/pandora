      subroutine WOLLO
     $(A,I,J,DZIJ,DZJ)
C
C     Rudolf Loeser, 1997 Aug 11
C---- Computes DZIJ = ZETA(I) - ZETA(J), and
C              DZJ  = ZETA(J) - ZETA(J-1),
C     using the component integrals in A from which ZETA was calculated,
C     rather than ZETA directly; here, J .ge. 2, and I .ge. J.
C     !DASH
      save
C     !DASH
      real*8 A, DZIJ, DZJ, ZERO
      integer I, J, K
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('WOLLO')
C     !BEG
      DZJ  = A(J)
      DZIJ = ZERO
C
      if(I.gt.J) then
        do 100 K = (J+1),I
          DZIJ = DZIJ+A(K)
  100   continue
      end if
C     !END
      call BYE ('WOLLO')
C
      return
      end
