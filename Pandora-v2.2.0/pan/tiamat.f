      subroutine TIAMAT
     $(M5,N3,WNR,W1)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Matrix manipulation for NIDABA.
C     WNR(2N+5,2N+5)  ->  W1(N+3,2N+5).
C     !DASH
      save
C     !DASH
      real*8 W1, WNR
      integer J, M5, N3
C     !DASH
      external MOVE1, HI, BYE
C
C               WNR(M5,M5), W1(N3,M5)
      dimension WNR(M5,*),  W1(N3,*)
C
      call HI ('TIAMAT')
C     !BEG
      do 100 J = 1,M5
        call MOVE1 (WNR(1,J),N3,W1(1,J))
  100 continue
C     !END
      call BYE ('TIAMAT')
C
      return
      end
