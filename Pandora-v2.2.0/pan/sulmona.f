      subroutine SULMONA
     $(J1,J2, P,NP, N,TC,TAUK,SCON,BK)
C
C     Rudolf Loeser, 1996 Feb 28
C---- Drives SIGTUNA, to pick subsets of TAUK, etc.
C     !DASH
      save
C     !DASH
      real*8 BK, P, SCON, TAUK, TC
      integer J1, J2, N, NP
C     !DASH
      external SIGTUNA, HI, BYE
C
C               P(N,5), TAUK(N,NB), SCON(N,NB), BK(N,NB)
      dimension P(*),   TAUK(N,*),  SCON(N,*),  BK(N,*)
C
      call HI ('SULMONA')
C     !BEG
      call SIGTUNA (N,TAUK(1,J1),BK(1,J1),SCON(1,J1),P,NP,TC)
      call SIGTUNA (N,TAUK(1,J2),BK(1,J2),SCON(1,J2),P,NP,TC)
C     !END
      call BYE ('SULMONA')
C
      return
      end
