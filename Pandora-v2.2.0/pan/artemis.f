      subroutine ARTEMIS
     $(N,NMX,C,XA,WN,XM)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Accumulates an angle contribution, for XM-calculation.
C     !DASH
      save
C     !DASH
      real*8 C, DELTA, WN, XA, XM
      integer I, J, N, NMX
C     !DASH
      external UNIT, HI, BYE
C
C               C(N), XA(N,N), WN(N,N), XM(NMX,NMX)
      dimension C(*), XA(N,*), WN(N,*), XM(NMX,*)
C
      call HI ('ARTEMIS')
C     !BEG
      do 101 J = 1,N
        do 100 I = 1,N
          call UNIT (I,J,DELTA)
          XM(I,J) = XM(I,J)+(C(I)*XA(I,J))*(WN(I,J)+DELTA)
  100   continue
  101 continue
C     !END
      call BYE ('ARTEMIS')
C
      return
      end
