      subroutine GOLLIAS
     $(SHE,V,N,NL,ADD)
C
C     Rudolf Loeser, 1998 Jul 23
C---- Computes an additional diffusion term for He-II, for COLUGO.
C     (This term contains He-I data.)
C     Only the first column of ADD is explicitly computed here.
C     !DASH
      save
C     !DASH
      real*8 ADD, SHE, V
      integer N, NL
C     !DASH
      external ZERO1, ARRMUL, TRINCA, HI, BYE
C
C               SHE(N), V(N), ADD(N,NL)
      dimension SHE(*), V(*), ADD(N,*)
C
      call HI ('GOLLIAS')
C     !BEG
C---- First, zero out everything
      call ZERO1  (ADD,(N*NL))
C---- Compute terms for level 1
      call ARRMUL (V,SHE,ADD(1,1),N)
      call TRINCA (ADD(1,1),N)
C     !END
      call BYE ('GOLLIAS')
C
      return
      end
