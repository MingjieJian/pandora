      subroutine AZALEA
     $(N,MRHO,IFS,ILS,WSM,WEIGHT,RHOO,RHOS,RHOJ,RHOW,WEIT,RHOIJ,IU,IL)
C
C     Rudolf Loeser, 1980 May 02
C---- Sets up "final" RHOs, for ANEMONE.
C     !DASH
      save
C     !DASH
      real*8 RHOIJ, RHOJ, RHOO, RHOS, RHOW, WEIGHT, WEIT, WSM
      integer IFS, IL, ILS, IU, MRHO, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external OLINDA, HALT, HI, BYE
C
C               WEIGHT(N), RHOO(N), RHOS(N), RHOJ(N), RHOW(N), WEIT(N),
      dimension WEIGHT(*), RHOO(*), RHOS(*), RHOJ(*), RHOW(*), WEIT(*),
C
C               RHOIJ(N)
     $          RHOIJ(*)
C
      call HI ('AZALEA')
C     !BEG
      if(MRHO.eq.0) then
        call OLINDA (RHOIJ, RHOO, RHOS,
     $               WEIGHT, N, IFS, ILS, WSM, IU, IL, WEIT)
C
      else if(MRHO.eq.1) then
        call OLINDA (RHOIJ, RHOO, RHOJ,
     $               WEIGHT, N, IFS, ILS, WSM, IU, IL, WEIT)
C
      else if(MRHO.eq.2) then
        call OLINDA (RHOIJ, RHOO, RHOW,
     $               WEIGHT, N, IFS, ILS, WSM, IU, IL, WEIT)
C
      else
        write (MSSLIN(1),100) MRHO
  100   format('MRHO =',I12,', which is neither 0, 1, nor 2.')
        call HALT ('AZALEA', 1)
      end if
C     !END
      call BYE ('AZALEA')
C
      return
      end
