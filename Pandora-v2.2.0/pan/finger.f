      subroutine FINGER
     $(SUM,ISIG,L1,L2,IU,ID)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Marks, if possible, for TAIFUN.
C     !DASH
      save
C     !DASH
      real*8 B1, B2, SUM
      integer ID, ISIG, IU, L1, L2, M1, M2
C     !DASH
      external HI, BYE
C
C               ISIG(N)
      dimension ISIG(*)
C
      data B1,B2 /6.D1, 9.D1/
      data M1,M2 /2, 3/
C
      call HI ('FINGER')
C     !BEG
      if((SUM.ge.B1).and.(L1.le.0)) then
        L1 = 1
        ISIG(IU) = M1
        ISIG(ID) = M1
      end if
      if((SUM.ge.B2).and.(L2.le.0)) then
        L2 = 1
        ISIG(IU) = M2
        ISIG(ID) = M2
      end if
C     !END
      call BYE ('FINGER')
C
      return
      end
