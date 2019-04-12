      subroutine ASPEN
     $(N,M,ZZ,MM,XM,V,IB)
C
C     Rudolf Loeser, 1968 Jun 04 (revised 2000 Jan 27)
C---- Special matrix multiplication routine.
C     It computes Z*XM=V, where XM is of order M*M and Z is of order
C     N*M. However, Z has only two non-zero elements in each row,
C     and is represented by the two vectors ZZ and MM, such that
C     MM(I) is the index of the first non-zero element in row I,
C     whose value is ZZ(I). The second non-zero element occurs at
C     MM(I)+1 (MM(I) .lt. M), and its value is 1-ZZ(I). (If MM(I)=M,
C     there is only one non-zero element, ZZ(I).)
C---- Furthermore,
C     as a side issue, we must add 1.0 to the first IB diagonal
C     elements of XM before we can use them.
C     !DASH
      save
C     !DASH
      real*8 ONE, V, X1, X2, XM, Z1, Z2, ZERO, ZZ
      integer I, IB, IZ, J, M, MM, N
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
      external HI, BYE
C
C               ZZ(N), MM(N), XM(M,M), V(N,M)
      dimension ZZ(*), MM(*), XM(M,*), V(N,*)
C     !EJECT
C
      call HI ('ASPEN')
C     !BEG
      do 101 I = 1,N
        IZ = MM(I)
        Z1 = ZZ(I)
C
        if(IZ.lt.M) then
          Z2 = ONE-Z1
        else
          Z2 = ZERO
        end if
C
        do 100 J = 1,M
          X1 = XM(IZ,J)
C
          if(IZ.lt.M) then
            X2 = XM(IZ+1,J)
          else
            X2 = ZERO
          end if
C
          if(J.le.IB) then
            if(IZ.eq.J) then
              X1 = X1+ONE
            else if((IZ+1).eq.J) then
              X2 = X2+ONE
            end if
          end if
C
          V(I,J) = Z1*X1 + Z2*X2
C
  100   continue
  101 continue
C     !END
      call BYE ('ASPEN')
C
      return
      end
