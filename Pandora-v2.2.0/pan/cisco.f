      subroutine CISCO
     $(ION,XNU,SIGMA)
C
C     Rudolf Loeser, 2006 Aug 25
C---- Computes cross-sections needed for C I-V CI(1) values,
C     according to Suno & Kato (2006).
C     !DASH
      save
C     !DASH
      real*8 A, CON, F, ONE, P, R, RLN, S, SIGMA, T, XI, XNU, XNUK,
     $       ZERO
      integer I, ION, J
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic max
C
      dimension XI(2,5), XNUK(2,5), A(5,2,5)
C
      data CON /1.D-13/
      data XI /
     $ 1.06D1, 0.D0,       2.44D1, 0.D0,       4.14D1, 0.D0,
     $ 6.45D1, 2.85D2,     3.92D2, 0.D0/
      data XNUK /
     $ 2.724D0, 0.D0,      5.896D0, 0.D0,      1.1579D1, 0.D0,
     $ 1.5595D1, 6.891D1,  9.4806D1, 0.D0/
      data A /
     $ 1.829D0, -1.975D0, 1.149D0, -3.583D0, 2.451D0,
     $ 5*0.D0,
     $ 0.839D0, -0.795D0, 3.263D0, -5.382D0, 3.476D0,
     $ 5*0.D0,
     $ 0.4009D0, -0.3518D0, 2.375D0, -3.992D0, 2.794D0,
     $ 5*0.D0,
     $ 1.35D0, -0.8748D0, -1.444D0, 2.33D0, -2.73D0,
     $ -2.777D0, 5.376D0, -8.748D0, 17.66D0, -9.086D0,
     $ 0.9205D0, -0.6297D0, 1.316D0, -0.09156D0, 0.D0,
     $ 5*0.D0/
C
      call HI ('CISCO')
C     !BEG
      SIGMA = ZERO
C
      if(((ION.ge.1).and.(ION.le.5)).and.(XNU.ne.ZERO)) then
        do 101 J = 1,2
          if(XNUK(J,ION).ne.ZERO) then
            if((J.eq.1).or.((J.eq.2).and.(XNU.ge.XNUK(J,4)))) then
              R   = XNUK(J,ION)/XNU
              F   = CON*(R/XI(J,ION))
              RLN = log(ONE/R)
              S   = A(1,J,ION)*RLN
              P   = ONE-R
              do 100 I = 2,5
                T = A(I,J,ION)*(P**(I-1))
                S = S+T
  100         continue
              S = F*S
C
              SIGMA = SIGMA+S
            end if
          end if
  101   continue
        SIGMA = max(SIGMA, ZERO)
      end if
C     !END
      call BYE ('CISCO')
C
      return
      end
