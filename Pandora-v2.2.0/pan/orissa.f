      subroutine ORISSA
     $(N,MRR,EM,C)
C
C     Rudolf Loeser, 1981 Nov 02
C---- Computes weights for integrations over Disk Rays.
C     (This is version 2 of ORISSA.)
C     !DASH
      save
C     !DASH
      real*8 C, EM, HALF
      integer I, J, MRR, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               EM(N,MRR), C(N,MRR)
      dimension EM(N,*),   C(N,*)
C
      call HI ('ORISSA')
C     !BEG
C---- First column
      J = 1
      do 100 I = 1,N
        C(I,J) = HALF*(EM(I,J)-EM(I,J+1))
  100 continue
C
      if(MRR.ge.3) then
C----   Interior columns
        do 102 J = 2,(MRR-1)
          do 101 I = 1,N
            C(I,J) = HALF*(EM(I,J-1)-EM(I,J+1))
  101     continue
  102   continue
      end if
C
C---- Last column
      J = MRR
      do 103 I = 1,N
        C(I,J) = HALF*(EM(I,J-1)-EM(I,J))
  103 continue
C     !END
      call BYE ('ORISSA')
C
      return
      end
