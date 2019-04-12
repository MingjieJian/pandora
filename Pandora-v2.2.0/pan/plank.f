      subroutine PLANK
     $(IU,IL,N,XNU,ALFIJ,T,BETAIJ,B)
C
C     Rudolf Loeser, 1981 Jul 27.
C---- Computes the Planck function values for transition (IU,IL).
C     (This is version 2 of PLANK.)
C     !DASH
      save
C     !DASH
      real*8 ALF, ALFIJ, B, BETAIJ, FNU, T, XNU
      integer I, IL, IU, IUL, N
C     !DASH
      external INDXUL, PLANCK, HI, BYE
C
C               XNU(NSL), ALFIJ(MUL), T(N), B(N), BETAIJ(N,MUL)
      dimension XNU(*),   ALFIJ(*),   T(*), B(*), BETAIJ(N,*)
C
      call HI ('PLANK')
C     !BEG
      call INDXUL   (IU, IL, IUL)
      ALF = ALFIJ(IUL)
      FNU = XNU(IU)-XNU(IL)
C
      do 100 I = 1,N
        call PLANCK (FNU, ALF, T(I), BETAIJ(I,IUL), B(I))
  100 continue
C     !END
      call BYE ('PLANK')
C
      return
      end
