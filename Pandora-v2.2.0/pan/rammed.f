      subroutine RAMMED
     $(K,ZL,ZR,M,ZA)
C
C     Rudolf Loeser, 2004 Jan 30
C---- Inserts added Z-vales, for RAGGED.
C     !DASH
      save
C     !DASH
      real*8 DV, DZ, XI, ZA, ZL, ZR
      integer I, K, M
C     !DASH
      external HI, BYE
C
C               ZA(N+MXTAP)
      dimension ZA(*)
C
      call HI ('RAMMED')
C     !BEG
      DZ = ZR-ZL
      DV = K+1
      do 100 I = 1,K
        XI = I
        M  = M+1
        ZA(M) = ZL+(XI/DV)*DZ
  100 continue
C     !END
      call BYE ('RAMMED')
C
      return
      end
