      subroutine GANE
     $(NLE,NSL)
C
C     Rudolf Loeser, 1992 Apr 06
C---- Checks input values of NLE.
C     !DASH
      save
C     !DASH
      integer J, NLE, NSL
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               NLE(NSL)
      dimension NLE(*)
C
      call HI ('GANE')
C     !BEG
      do 100 J = 1,NSL
        NLE(J) = max(NLE(J),1)
  100 continue
C     !END
      call BYE ('GANE')
C
      return
      end
