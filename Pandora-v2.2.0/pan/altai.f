      subroutine ALTAI
     $(I,N,PAR,WN,D,E,S,R)
C
C     Rudolf Loeser, 1981 Jun 29
C---- Computes elements to the left of the diagonal for "GR" weight
C     matrix calculation.
C     (See also GYDAN.)
C     !DASH
      save
C     !DASH
      real*8 D, E, EL, HALF, ONE, R, S, TWO, WN, ZERO
      integer I, J, N
      logical CP, PAR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               WN(N,N), D(N), E(N), S(N), R(N)
      dimension WN(N,*), D(*), E(*), S(*), R(*)
C     !EJECT
C
      call HI ('ALTAI')
C     !BEG
      EL = ONE
      CP = .true.
C
      J = I
  100 continue
        J = J-1
        if(J.gt.0) then
C
          if(PAR.and.CP) then
            CP = .false.
            WN(I,J) =  (TWO-(ONE+HALF*D(I))*E(I)
     $                -((ONE+HALF*D(I))*(ONE+D(I-1))
     $                +HALF*(D(I-1)**2))*E(I-1))/(D(I-1)*(D(I-1)+D(I)))
          else
            WN(I,J) = HALF*EL*S(J)
          end if
C
          if(J.gt.1) then
            EL = EL*E(J)
            if(EL.gt.ZERO) then
              WN(I,J) = WN(I,J)+HALF*EL*R(J-1)
              goto 100
            end if
          end if
C
        end if
      continue
C     !END
      call BYE ('ALTAI')
C
      return
      end
