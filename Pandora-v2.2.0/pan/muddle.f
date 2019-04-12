      subroutine MUDDLE
     $(XN,E,G,FJIN,FJINL,XQ,F,A,NMX,XQMAX,DQMIN,DQMAX,RACC,CUT,DUMP)
C
C     Rudolf Loeser, 1984 Jul 09
C---- Computes a value of the injection function FJIN, and
C     of the intermediate term G, for MINCH.
C     !DASH
      save
C     !DASH
      real*8 A, CUT, DQMAX, DQMIN, E, F, FAC, FJIN, FJINL, G, ONE, PI,
     $       RACC, RE, TWO, XN, XQ, XQMAX, XQMIN, ZERO
      integer N, NMX
      logical DUMP
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external CHUCK, FIXIT, ALBITE, HELENA, HALT, HI, BYE
C
C               XQ(NMX), F(NMX), A(NMX)
      dimension XQ(*),   F(*),   A(*)
C
      data FAC /1024.D0/
C
      call HI ('MUDDLE')
C     !BEG
C---- Compute XQMIN
      RE    = sqrt(TWO*E)
      XQMIN = (ONE-ONE/(XN**2))/(TWO*RE)
C
C---- Set up tables of XQ and F
      call FIXIT    (N, NMX, XQ, F, CHUCK, XN, XQMIN, XQMAX, DQMIN,
     $               DQMAX, RACC, CUT)
      if(N.le.0) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not greater than 0.')
        call HALT   ('MUDDLE', 1)
      end if
C
C---- Integrate, to get G
      call HELENA   (XQ, 1, F, 1, A, 1, N, G)
C
      if(DUMP) then
        call ALBITE (XN, XQ, F, A, N)
      end if
C
C---- Compute FJIN, and its log
      FJIN = (((FAC*PI)/E))*(ONE/(XN**3))*G
C
      if(FJIN.gt.ZERO) then
        FJINL = log(FJIN)
      else
        FJINL = ZERO
      end if
C     !END
      call BYE ('MUDDLE')
C
      return
      end
