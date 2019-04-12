      subroutine PAVIA
     $(LU,N,NL,RHOIJ,BDIJ,KIJ)
C
C     Rudolf Loeser, 1996 Apr 18
C---- Prints final sets of RHO and BD, for TULIP.
C     !DASH
      save
C     !DASH
      real*8 BDIJ, RHOIJ
      integer KIJ, LU, N, NL
C     !DASH
      external LINER, SCRIBE, MEETOO, HI, BYE
C
C               RHOIJ(N,NT), BDIJ(N,NL), KIJ(NL,NL)
      dimension RHOIJ(*),    BDIJ(*),    KIJ(*)
C
      call HI ('PAVIA')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2, LU)
        write (LU,100)
  100   format(' ','Final values of RHO: Net Radiative Bracket')
        call SCRIBE (RHOIJ, 'NT', KIJ, 1, N, N, NL, LU, 1)
C
        call LINER  (2, LU)
        write (LU,101)
  101   format(' ','Final values of RBD: Ratios of Departure ',
     $             'Coefficients')
        call MEETOO (BDIJ, 1, N, NL, LU)
      end if
C     !END
      call BYE ('PAVIA')
C
      return
      end
