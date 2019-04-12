      subroutine HUNK
     $(T,X,MODE,ARG)
C     Rudolf Loeser, 1973 Sep 04
C---- Computes the argument of the
C     exponential temperature decay term: (h*nu/k*T).
C     T is in Kelvins.
C
C     When MODE=1: then X is in frequency units;
C          MODE=2: then X is in Angstroms.
C
C     !DASH
      save
C     !DASH
      real*8 ARG, CON4, CON5, RAT, T, X
      integer MODE
      logical KILROY
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
      data KILROY /.true./
C
      call HI ('HUNK')
C     !BEG
      if(KILROY) then
        call RIGEL  (4, CON4)
        call RIGEL  (5, CON5)
        KILROY = .false.
      end if
C
      if(MODE.eq.1) then
        call DIVIDE (X, T, RAT)
        ARG = CON4*RAT
C
      else if(MODE.eq.2) then
        call DIVIDE (CON5, (X*T), ARG)
C
      end if
C     !END
      call BYE ('HUNK')
C
      return
      end
