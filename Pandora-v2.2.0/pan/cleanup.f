      subroutine CLEANUP
     $(A,N,NMAX,CRITJ)
C
C     Rudolf Loeser, 1990 Dec 12
C---- Edits junk out of A, a square matrix of order N x N.
C     !DASH
      save
C     !DASH
      real*8 A, CRITJ, CUT, XTRM, ZERO
      integer I, J, N, NMAX
      logical YES
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  IS_BAD, HI, BYE
      intrinsic max,abs
C
C               A(NMAX,N)
      dimension A(NMAX,*)
C
      call HI ('CLEANUP')
C     !BEG
      if(N.gt.1) then
        do 103 I = 1,N
C
          do 100 J = 1,N
            call IS_BAD (A(I,J), YES)
            if(YES) then
              A(I,J) = ZERO
            end if
  100     continue
C
          XTRM = abs(A(I,1))
          do 101 J = 2,N
            XTRM = max(abs(A(I,J)),XTRM)
  101     continue
C
          CUT = CRITJ*XTRM
          do 102 J = 1,N
            if(abs(A(I,J)).lt.CUT) then
              A(I,J) = ZERO
            end if
  102     continue
C
  103   continue
      end if
C     !END
      call BYE ('CLEANUP')
C
      return
      end
