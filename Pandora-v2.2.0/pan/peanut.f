      subroutine PEANUT
     $(N,XNE,SAB,U,HNP)
C
C     Rudolf Loeser, 1975 Jul 29
C---- Computes new XNE, for SHERBET.
C     !DASH
      save
C     !DASH
      real*8 ARG, C1, C2, CRIT, CWARTR, EIGHTH, FOUR, FSB, HNP, ONE, S,
     $       SAB, T, TRM, TU, TWO, U, U2, XNE, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(18),EIGHTH)
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
C     !DASH
C     !EJECT
      external DIVIDE, CRUST, HI, BYE
C
C               XNE(N), SAB(N), U(N), HNP(N)
      dimension XNE(*), SAB(*), U(*), HNP(*)
C
      data CRIT,C1,C2 /1.D-2, 7.8125D-2, 5.46875D-2/
C
      call HI ('PEANUT')
C     !BEG
      do 100 I = 1,N
        FSB = FOUR*SAB(I)
        U2  = U(I)**2
        TRM = TWO*FSB*HNP(I)
        call DIVIDE    (TRM, U2, TU)
C
        if((U(I).le.ZERO).or.(TU.ge.CRIT)) then
          ARG = U2+TRM
          if(ARG.lt.ZERO) then
C           Error message, and abort
            call CRUST (N, SAB, U, HNP, I)
          end if
          S = sqrt(ARG)
          call DIVIDE  ((S-U(I)), FSB, XNE(I))
        else
          T = (ONE-TU*(CWARTR-TU*(EIGHTH-TU*(C1-TU*C2))))
          call DIVIDE  ((T*HNP(I)), U(I), XNE(I))
        end if
C
  100 continue
C     !END
      call BYE ('PEANUT')
C
      return
      end
