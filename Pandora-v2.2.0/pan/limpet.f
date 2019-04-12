      subroutine LIMPET
     $(N,Z,HK,H1,ZI,ZION,XION,W,IW)
C
C     Rudolf Loeser, 1992 Feb 21
C---- Computes Hydrogen ionization terms, for diffusion calculations.
C     (This is version 2 of LIMPET.)
C     !DASH
      save
C     !DASH
      real*8 H1, HK, W, XION, Z, ZI, ZION
      integer IW, N
C     !DASH
      external ARRDIV, SIGMA, LEPUS, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), HK(N), H1(N), ZI(N), ZION(N), XION(N)
      dimension Z(*), HK(*), H1(*), ZI(*), ZION(*), XION(*)
C
      call HI ('LIMPET')
C     !BEG
C---- XION: Hydrogen ionization ratio
      call ARRDIV (HK,H1,XION,N)
C---- ZI:   logarithmic derivative of XION
      call SIGMA  (N,Z,XION,ZI,'ZI',W,IW)
C---- ZION: multiplier based on XION
      call LEPUS  (N,XION,ZION)
C     !END
      call BYE ('LIMPET')
C
      return
      end
