      subroutine RIB
     $(N,NT,ITMX,IU,IL,X,A)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Pulls out a subset, for LAZICA.
C     !DASH
      save
C     !DASH
      real*8 A, X
      integer IL, ITMX, IU, IUL, J, N, NT
C     !DASH
      external INTRANS, MOVE1, HI, BYE
C
C               X(N,NT,ITMX), A(N,ITMX)
      dimension X(N,NT,*),    A(N,*)
C
      call HI ('RIB')
C     !BEG
      call INTRANS (IU,IL,'RIB',IUL)
      do 100 J = 1,ITMX
        call MOVE1 (X(1,IUL,J),N,A(1,J))
  100 continue
C     !END
      call BYE ('RIB')
C
      return
      end
