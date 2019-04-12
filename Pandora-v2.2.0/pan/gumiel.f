      subroutine GUMIEL
     $(NSL,MRJ,WRAT,RRCPIJ,YRATE)
C
C     Rudolf Loeser, 1982 Nov 24
C---- Initializes parameters for rates integrations.
C     (This is version 2 of GUMIEL.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RRCPIJ, WRAT, Y, YRATE, ZERO
      integer J, K, L, MRJ, NSL, jummy
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
      external MINK, BLIND, ZERO1, SET1, HI, BYE
C
C               MRX = MRS+NSL+1
C
C               MRJ(NSL+1), WRAT(MRX), RRCPIJ(MRX), YRATE(MRX)
      dimension MRJ(*),     WRAT(*),   RRCPIJ(*),   YRATE(*)
C
C
      call HI ('GUMIEL')
C     !BEG
      call BLIND     (-ONE, Y)
      do 100 J = 1,(NSL+1)
        call MINK    (J, MRJ, jummy, K)
C
        WRAT(K) = ONE
C       (If needed by HUGRA, the actual value of WRAT(K), the
C       wavelength at the head of the J'th continuum, will be computed)
        RRCPIJ(K) = -ONE
C       (HUGRA takes another look at RRCPIJ(K); if it still
C       equals -1, then its value is changed to +1)
        YRATE(K) = Y
C
        L = MRJ(J)
        if(L.gt.0) then
          call ZERO1 (WRAT  (K+1), L)
          call ZERO1 (RRCPIJ(K+1), L)
          call SET1  (YRATE (K+1), L, Y)
        end if
  100 continue
C     !END
      call BYE ('GUMIEL')
C
      return
      end
