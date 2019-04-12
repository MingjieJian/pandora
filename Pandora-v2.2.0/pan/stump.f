      subroutine STUMP
     $(NE,NZ,S,ERT)
C
C     Rudolf Loeser, 1974 Dec 13
C---- Extends S for TAURUS.
C     !DASH
      save
C     !DASH
      real*8 ERT, S
      integer J, NE, NUM, NZ
C     !DASH
      external MOVE1, HI, BYE
C
C               S(NZ), ERT(NZ)
      dimension S(*),  ERT(*)
C
      call HI ('STUMP')
C     !BEG
      NUM = NZ-NE
      if(NUM.gt.0) then
        J = NE+1
        call MOVE1 (ERT(J),NUM,S(J))
      end if
C     !END
      call BYE ('STUMP')
C
      return
      end
