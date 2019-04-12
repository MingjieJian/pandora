      subroutine MAXIM
     $(DL,DW,XIK,N,K)
C
C     Rudolf Loeser, 1978 Apr 23
C---- Sets up XIK for PRD printouts.
C     !DASH
      save
C     !DASH
      real*8 DL, DW, XIK
      integer I, J, K, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               DW(N), DL(K), XIK(N,K)
      dimension DW(*), DL(*), XIK(N,*)
C
      call HI ('MAXIM')
C     !BEG
      do 101 J = 1,K
        do 100 I = 1,N
          call DIVIDE (DL(J),DW(I),XIK(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('MAXIM')
C
      return
      end
