      subroutine TANCRED
     $(A,INC,N,SUM)
C
C     Rudolf Loeser, 1994 Oct 25
C---- Computes a "logarithmic" checksum of N elements of array A,
C     beginning with A(1), using stride of length INC.
C     !DASH
      save
C     !DASH
      real*8 A, AA, AL, SUM, ZERO
      integer I, INC, J, N
      logical BAD
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
      intrinsic abs
C
C               A(INC*(N-1)+1)
      dimension A(*)
C
      call HI ('TANCRED')
C     !BEG
      SUM = ZERO
      if(N.gt.0) then
        J = 1-INC
        do 100 I = 1,N
          J = J+INC
          if(A(J).ne.ZERO) then
            AA = abs(A(J))
            call IS_BAD (AA, BAD)
            if(.not.BAD) then
              AL  = log10(AA)
              SUM = SUM+AL
            end if
          end if
  100   continue
      end if
C     !END
      call BYE ('TANCRED')
C
      return
      end
