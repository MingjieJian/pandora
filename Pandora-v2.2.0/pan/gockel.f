      subroutine GOCKEL
     $(N,FMV,GX1M)
C
C     Rudolf Loeser, 1997 May 06
C---- Computes GX1M for velocity term in the diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 CRIT, FMV, GX1M, ONE, ZERO
      integer I, N
      logical KLL, KLP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               FMV(N), GX1M(N)
      dimension FMV(*), GX1M(*)
C
      data CRIT /0.999D0/
C
      call HI ('GOCKEL')
C     !BEG
      KLL = FMV(1).lt.CRIT
      do 100 I = 1,(N-1)
        KLP = KLL
        KLL = FMV(I+1).lt.CRIT
        if(KLP.or.KLL) then
          GX1M(I) = ZERO
        else
          GX1M(I) = ONE
        end if
  100 continue
C
      if(KLL) then
        GX1M(N) = ZERO
      else
        GX1M(N) = ONE
      end if
C     !END
      call BYE ('GOCKEL')
C
      return
      end
