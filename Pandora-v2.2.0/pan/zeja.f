      subroutine ZEJA
     $(I,N,PAR,WN,D,E,P)
C
C     Rudolf Loeser, 1989 Jun 30
C---- Computes diagonal term for "GR" weight matrix calculation.
C     (See also ZIYA.)
C     (This is version 2 of ZEJA.)
C     !DASH
      save
C     !DASH
      real*8 D, E, HALF, ONE, P, TWO, WN
      integer I, N
      logical PAR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               WN(N,N), D(N), E(N), P(N)
      dimension WN(N,*), D(*), E(*), P(*)
C
      call HI ('ZEJA')
C     !BEG
      if(I.eq.1) then
        WN(I,I) = -HALF*(ONE+P(I))
C
      else if(I.lt.N) then
        if(PAR) then
          WN(I,I) = -(TWO-(ONE+HALF*(D(I-1)+D(I)))*(E(I-1)+E(I)))
     $               /(D(I-1)*D(I))
        else
          WN(I,I) = -HALF*(P(I-1)+P(I))
        end if
C
      else
        WN(I,I) = -HALF*(P(I-1)+ONE)
      end if
C     !END
      call BYE ('ZEJA')
C
      return
      end
