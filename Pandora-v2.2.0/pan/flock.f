      subroutine FLOCK
     $(T,E,PR,CHI,FL)
C
C     Rudolf Loeser, 1972 Nov 14
C---- Computes FL, a term for LTE populations calculation.
C     FL is the number of ions of ionization stage i+1
C           divided by the number in stage i.
C     !DASH
      save
C     !DASH
      real*8 ARG, CHI, CON23, CON24, E, F, FL, P, POE, PR, S, T
      logical KILROY
C     !DASH
      external DIVIDE, RIGEL, HI, BYE
C
      data KILROY /.true./
C
      call HI ('FLOCK')
C     !BEG
      if(KILROY) then
        call RIGEL (24, CON24)
        call RIGEL (23, CON23)
        KILROY = .false.
      end if
C
      call DIVIDE  ((CON24*CHI), T, ARG)
      P = exp(-ARG)
      call DIVIDE  (P, E, POE)
      S = sqrt(T)
      F = (S**3)*POE
C
      FL = (PR/CON23)*F
C     !END
      call BYE ('FLOCK')
C
      return
      end
