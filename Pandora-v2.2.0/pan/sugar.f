      subroutine SUGAR
     $(N,ITMX,A)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Normalizes sets of iterates.
C     (This is version 2 of SUGAR.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, ITMX, J, N
C     !DASH
      external  SCARAB, HI, BYE
C
C               A(N,ITMX)
      dimension A(N,*)
C
C
      call HI ('SUGAR')
C     !BEG
      do 101 J = 1,(ITMX-1)
        do 100 I = 1,N
          call SCARAB (A(I,J),A(I,ITMX),A(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('SUGAR')
C
      return
      end
