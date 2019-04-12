      subroutine LEO
     $(ZI,Z1,Z2,Z3,ZT,D,VAMB,VBMB,VCMB,VDMB,VEMB)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Computes velocities for AQUILA.
C     (This is version 2 of LEO.)
C     !DASH
      save
C     !DASH
      real*8 D, VAMB, VBMB, VCMB, VDMB, VEMB, Z1, Z2, Z3, ZI, ZT
C     !DASH
      external HI, BYE
C
      dimension D(4,5)
C
      call HI ('LEO')
C     !BEG
      VAMB = ZI*D(1,1) +Z1*D(1,2) +Z2*D(1,3) +Z3*D(1,4) +ZT*D(1,5)
      VBMB = ZI*D(2,1) +Z1*D(2,2) +Z2*D(2,3) +Z3*D(2,4) +ZT*D(2,5)
      VCMB = ZI*D(3,1) +Z1*D(3,2) +Z2*D(3,3) +Z3*D(3,4) +ZT*D(3,5)
      VDMB = ZI*D(4,1) +Z1*D(4,2) +Z2*D(4,3) +Z3*D(4,4) +ZT*D(4,5)
      VEMB =                                  Z3*D(3,3)
C     !END
      call BYE ('LEO')
C
      return
      end
