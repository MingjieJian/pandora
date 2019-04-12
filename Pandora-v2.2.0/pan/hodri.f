      subroutine HODRI
     $(A,B,N)
C
C     Rudolf Loeser, 1979 Oct 31
C---- Sets default values of B.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer N
C     !DASH
      external REFILL, HI, BYE
C
C               A(N), B(N)
      dimension A(*), B(*)
C
      call HI ('HODRI')
C     !BEG
      call REFILL (A,B,N)
C     !END
      call BYE ('HODRI')
C
      return
      end
