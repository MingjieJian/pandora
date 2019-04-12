      subroutine DULBA
     $(J,TE,XNUC,XNU,NPQ,LRQ,NLE,CI)
C
C     Rudolf Loeser, 1992 Mar 13
C---- Computes CI, collisional ionization coefficient, based on
C     Coulomb-Born cross-sections according to
C
C     Clark, Abdallah, and Mann (1991), Ap.J 381, 597.
C     !DASH
      save
C     !DASH
      real*8 C, CI, CON4, CON64, CRIT, DNU, E1, E1E, E2, E2E, ELL, EMY,
     $       ENL, ENN, F, FAC, FOUR, ONE, RT, SIX, T1, T2, T3, TE,
     $       THREE, THRTEEN, TWO, XNU, XNUC, Y, ZERO
      integer J, LRQ, NLE, NPQ
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 7),SIX   )
C     !DASH
C     !EJECT
      external RIGEL, EXPINT, DIVIDE, CURLY, HI, BYE
C
C               XNUC(NSL), XNU(NSL), NPQ(NSL), LRQ(NSL), NLE(NSL)
      dimension XNUC(*),   XNU(*),   NPQ(*),   LRQ(*),   NLE(*)
C
      dimension C(9)
      data      C / 1.5369D0,   9.9656D-1, -6.1916D-1,  2.4463D0,
     $             -2.4773D0,   3.2151D0,  -1.4512D0,   1.7230D0,
     $             -4.7075D-1/
C
      data FAC,THRTEEN,CRIT /5.48D-11, 13.D0, 250.D0/
      data KILROY /.true./
C
      call HI ('DULBA')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL  ( 4, CON4)
        call RIGEL  (64, CON64)
      end if
C
      ENN = NPQ(J)
      ELL = LRQ(J)
      ENL = NLE(J)
      if(ELL.eq.-ONE) then
        ELL = ((ENN-ONE)*(FOUR*ENN+ONE))/(SIX*ENN)
      else if((ELL.eq.-THREE).and.(ENN.gt.THREE)) then
        ELL = ((ENN*(ENN-ONE)*(FOUR*ENN+ONE))/SIX-THRTEEN)
     $        /((ENN**2)-THREE)
      end if
C
      DNU = XNUC(J)-XNU(J)
      Y   = CON4*(DNU/TE)
      if(Y.lt.CRIT) then
        call EXPINT (1, Y, E1, EMY)
        call EXPINT (2, Y, E2, EMY)
        call DIVIDE (E1, EMY, E1E)
        call DIVIDE (E2, EMY, E2E)
      else
        call CURLY  (Y, E1E, E2E)
      end if
      RT = sqrt(TE)
C
      F  = FAC*RT*Y*((CON64/DNU)**2)
      T1 = (C(1)+(C(2)+C(3)*ELL)/ENN)*E1E
      T2 = (C(4)+(C(5)+C(6)*ELL)/ENN)*(ONE-Y*E1E)
      T3 = (C(7)+(C(8)+C(9)*ELL)/ENN)*(ONE-TWO*Y*E1E+Y*E2E)
C
      CI = ENL*F*(T1+T2+T3)
C     !END
      call BYE ('DULBA')
C
      return
      end
