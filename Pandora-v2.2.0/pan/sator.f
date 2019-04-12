      subroutine SATOR
     $(A,IB,IE,ARR,M)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Selects array members to be printed, for TOAD.
C     (See also JATOR.)
C     (This is version 2 of SATOR.)
C     !DASH
      save
C     !DASH
      real*8 A, ARR
      integer IB, IE, M
C     !DASH
      external ZERO1, MOVE1, HI, BYE
C
C               A(*), ARR(8)
      dimension A(*), ARR(*)
C
      call HI ('SATOR')
C     !BEG
      call ZERO1 (ARR, 8)
      M = (IE-IB)+1
      call MOVE1 (A(IB), M, ARR(2))
      M = M+1
C     !END
      call BYE ('SATOR')
C
      return
      end
