      subroutine THELA
     $(C,N,A,SUM)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Accumulates an angle contribution, for Jnu-calculation.
C     Note: SUM is initialized by the caller.
C     !DASH
      save
C     !DASH
      real*8 A, C, SUM
      integer I, N
C     !DASH
      external HI, BYE
C
C               A(N,N), SUM(N), C(N)
      dimension A(N,*), SUM(*), C(*)
C
      call HI ('THELA')
C     !BEG
      do 100 I = 1,N
        SUM(I) = SUM(I)+C(I)*A(I,I)
  100 continue
C     !END
      call BYE ('THELA')
C
      return
      end
