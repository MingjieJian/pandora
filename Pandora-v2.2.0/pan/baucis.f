      subroutine BAUCIS
     $(I,K,MUX,TMU,Z,N,ZTM)
C
C     Rudolf Loeser, 1992 Dec 30
C---- Sets up ZTM, the array of depth values used by BROOM (q.v.).
C     (This is version 6 of BAUCIS.)
C     !DASH
      save
C     !DASH
      real*8 TMU, VAL, Z, ZTM
      integer I, IRET, J, K, MUX, N
C     !DASH
      external  LININT, HI, BYE
C
C               TMU(N), Z(N), ZTM(KM,4)
      dimension TMU(*), Z(*), ZTM(K ,*)
C
      dimension VAL(3)
C
      data VAL /1.D-1,1.D0,1.D1/
C
      call HI ('BAUCIS')
C     !BEG
      ZTM(I,1)=Z(MUX)
C
      do 100 J = 2,4
        call LININT (TMU,1,Z,1,N, VAL(J-1),ZTM(I,J), 1,1,IRET)
  100 continue
C     !END
      call BYE ('BAUCIS')
C
      return
      end
