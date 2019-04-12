      subroutine GENOA
     $(NO,N,HNDO,HNDN,XNEO,XNEN,RAT)
C
C     Rudolf Loeser, 1982 Jul 09
C---- Supplementary HSE printout.
C     !DASH
      save
C     !DASH
      real*8 HNDN, HNDO, RAT, XNEN, XNEO
      integer N, NO
      logical VZERO
      character LINE*127
C     !DASH
      external ARRDIV, LINER, LABFIL, VECOUT, HI, BYE
C
C               HNDO(N), HNDN(N), XNEO(N), XNEN(N), RAT(N)
      dimension HNDO(*), HNDN(*), XNEO(*), XNEN(*), RAT(*)
C
      call HI ('GENOA')
C     !BEG
      call LINER  (2, NO)
C
      call ARRDIV (HNDN, HNDO, RAT, N)
      call LABFIL ('New/Old, Hydrogen number density', LINE)
      call VECOUT (NO, RAT, N, LINE)
C
      call ARRDIV (XNEN, XNEO, RAT, N)
      call LABFIL ('New/Old, Electron number density', LINE)
      call VECOUT (NO, RAT, N, LINE)
C     !END
      call BYE ('GENOA')
C
      return
      end
