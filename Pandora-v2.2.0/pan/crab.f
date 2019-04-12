      subroutine CRAB
     $(A,IB,NL,NSL,ARR,M)
C
C     Rudolf Loeser, 1978 Jul 28
C---- Drives SATOR with special case, for TOAD.
C     !DASH
      save
C     !DASH
      real*8 A, ARR
      integer IB, IE, M, NL, NSL
C     !DASH
      external  SATOR, HI, BYE
      intrinsic min
C
C               A(NSL), ARR(8)
      dimension A(*),   ARR(*)
C
      call HI ('CRAB')
C     !BEG
      if((NSL.gt.NL).and.(IB.le.NL)) then
        IE = min(IB+6,NL)
        call SATOR (A,IB,IE,ARR,M)
      end if
C     !END
      call BYE ('CRAB')
C
      return
      end
