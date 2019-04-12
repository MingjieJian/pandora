      subroutine ZIZANIA
     $(ITAU,N,BDI,GMI,PIJIJ,PIJJI,UU,X,Y,I,J)
C
C     Rudolf Loeser, 1968 MAR 26
C---- Computes X and Y for ORYZA, for the case W .ne. 0.
C     !DASH
      save
C     !DASH
      real*8 B, BDI, F, GMI, HALF, ONE, PIJIJ, PIJJI, R, TWO, UU, W, X,
     $       Y, ZERO
      integer I, ITAU, J, N
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
      external DIVIDE, HI, BYE
C
C               GMI(N,NSL), BDI(N,NL)
      dimension GMI(N,*),   BDI(N,*)
C
      call HI ('ZIZANIA')
C     !BEG
      call DIVIDE   (BDI(ITAU,I), BDI(ITAU,J), B)
C
      W = UU
      if(I.lt.J) then
        W = -W
      end if
C
      F = HALF*(ONE+W)
C
      X = F*(TWO-W*(ONE+B))
C
      R  = GMI(ITAU,J)*PIJJI
      if(R.eq.ZERO) then
        Y = F*(TWO-W)
      else
        call DIVIDE (R, (GMI(ITAU,I)*PIJIJ), R)
        Y = F*(TWO-W*(ONE+R*B))
      end if
C     !END
      call BYE ('ZIZANIA')
C
      return
      end
