      subroutine STRIKE
     $(M,N,XM,XS)
C
C     Rudolf Loeser, 1968 May 06
C---- Obtains XS from XM by deleting the first row and the M'th column.
C     !DASH
      save
C     !DASH
      real*8 XM, XS
      integer J, K, M, N
C     !DASH
      external MOVE1, HI, BYE
C
C               XM(N,N), XS((N-1),(N-1))
      dimension XM(N,*), XS((N-1),*)
C
      call HI ('STRIKE')
C     !BEG
      K = 0
      do 100 J = 1,N
        if(J.ne.M) then
          K = K+1
          call MOVE1 (XM(2,J),(N-1),XS(1,K))
        end if
  100 continue
C     !END
      call BYE ('STRIKE')
C
      return
      end
