      subroutine SAMOYED
     $(N,D,E,W,R)
C
C     Rudolf Loeser, 1981 Jun 21
C---- Computes auxiliaries for "GR" weight matrix calculations.
C     !DASH
      save
C     !DASH
      real*8 D, E, R, W
      integer J, N
C     !DASH
      external QEXP3, QEXP2, HI, BYE
C
C               D(N), E(N), W(N), R(N)
      dimension D(*), E(*), W(*), R(*)
C
      call HI ('SAMOYED')
C     !BEG
      do 100 J = 1,N
        E(J) = exp(-D(J))
        call QEXP3 (D(J), E(J), 1, W(J))
        call QEXP2 (D(J), E(J), 1, R(J))
  100 continue
C     !END
      call BYE ('SAMOYED')
C
      return
      end
