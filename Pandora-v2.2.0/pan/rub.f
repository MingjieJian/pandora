      subroutine RUB
     $(N,LIM,ITMX,J,X,A)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Pulls out a subset, for THRUSH.
C     !DASH
      save
C     !DASH
      real*8 A, X
      integer I, ITMX, J, LIM, N
C     !DASH
      external MOVE1, HI, BYE
C
C               X(N,LIM,NNDITR), A(N,ITMX)
      dimension X(N,LIM,*),      A(N,*)
C
      call HI ('RUB')
C     !BEG
      do 100 I = 1,ITMX
        call MOVE1  (X(1,J,I),N,A(1,I))
  100 continue
C     !END
      call BYE ('RUB')
C
      return
      end
