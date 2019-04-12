      subroutine TAYMYR
     $(I,N,PAR,WH,D,E,S,R)
C
C     Rudolf Loeser, 1989 Jun 28
C---- Computes elements to the right of the diagonal, for PHI
C     operator calculation.
C     (See also BAIKAL.)
C     !DASH
      save
C     !DASH
      real*8 D, E, ER, HALF, ONE, R, S, TWO, WH, ZERO
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
C               WH(N,N), D(N), E(N), S(N), R(N)
      dimension WH(N,*), D(*), E(*), S(*), R(*)
C     !EJECT
C
      call HI ('TAYMYR')
C     !BEG
      ER = ONE
      CP = .true.
C
      J = I
  100 continue
        J = J+1
        if(J.le.N) then
C
          if(PAR.and.CP) then
            CP = .false.
            WH(I,J) =  HALF*(TWO*D(I-1)-(TWO+D(I-1)+TWO*D(I)
     $                +D(I-1)*D(I)+D(I)**2)*E(I)+(TWO+D(I-1))*E(I-1))
     $                /(D(I)*(D(I)+D(I-1)))
          else
            WH(I,J) = HALF*ER*S(J-1)
          end if
C
          if(J.lt.N) then
            ER = ER*E(J-1)
            if(ER.gt.ZERO) then
              WH(I,J) = WH(I,J)+HALF*ER*R(J)
              goto 100
            end if
          end if
C
        end if
      continue
C     !END
      call BYE ('TAYMYR')
C
      return
      end
