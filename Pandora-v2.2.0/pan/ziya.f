      subroutine ZIYA
     $(I,N,PAR,WH,D,E,R)
C
C     Rudolf Loeser, 1989 Jun 30
C---- Computes diagonal term, for PHI operator calculation.
C     (See also ZEJA.)
C     (This is version 2 of ZIYA.)
C     !DASH
      save
C     !DASH
      real*8 D, E, HALF, R, TWO, WH
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
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               WH(N,N), D(N), E(N), R(N)
      dimension WH(N,*), D(*), E(*), R(*)
C
      call HI ('ZIYA')
C     !BEG
      if(I.eq.1) then
        WH(I,I) = HALF*(R(I))
C
      else if(I.lt.N) then
        if(PAR) then
          WH(I,I) = HALF*(TWO*(D(I)-D(I-1))+(TWO+D(I-1)+D(I))
     $              *(E(I)-E(I-1)))/(D(I-1)*D(I))
        else
          WH(I,I) = HALF*(R(I)-R(I-1))
        end if
C
      else
        WH(I,I) = HALF*(-R(I-1))
      end if
C     !END
      call BYE ('ZIYA')
C
      return
      end
