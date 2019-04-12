      subroutine RUGILA
     $(N,NSHL,CSHL,XASHL,WNSHL,MRR,CDSK,XADSK,WNDSK,RR,XJNUO,SM1,SM2,
     $ XJNU,ITER,DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes new XJNU from previous XJNU, for angle-dependent
C     Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CSHL, ONE, RAT, RR, SM1, SM2, WNDSK, WNSHL, XADSK,
     $       XASHL, XDEN, XJNU, XJNUO, XNUM, ZERO
      integer I, ITER, M, MRR, N, NSHL
      logical DUMP
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
      external  ZERO1, ODEON, THELA, TATAR, BLEDA, DIVIDE, HI, BYE
      intrinsic max
C
C               CSHL(N,NSHL), XASHL(N,N,NSHL), WNSHL(N,N,NSHL), SM1(N),
      dimension CSHL(N,*),    XASHL(N,N,*),    WNSHL(N,N,*),    SM1(*),
C
C               CDSK(N,MRR ), XADSK(N,N,MRR ), WNDSK(N,N,MRR ), SM2(N),
     $          CDSK(N,*),    XADSK(N,N,*),    WNDSK(N,N,*),    SM2(*),
C
C               RR(N), XJNU(N), XJNUO(N)
     $          RR(*), XJNU(*), XJNUO(*)
C     !EJECT
C
      call HI ('RUGILA')
C     !BEG
C---- Compute SM1 and SM2
      call ZERO1    (SM1, N)
      call ZERO1    (SM2, N)
C
C     Shell part
      I = 0
      do 100 M = 1,NSHL
        call TATAR  (I)
        call ODEON  (CSHL(1,M), I, WNSHL(1,1,M), XASHL(1,1,M), XJNUO,
     $               SM1)
        call THELA  (CSHL(1,M), I, XASHL(1,1,M), SM2)
  100 continue
C
C     Disk part
      do 101 M = 1,MRR
        call ODEON  (CDSK(1,M), N, WNDSK(1,1,M), XADSK(1,1,M), XJNUO,
     $               SM1)
        call THELA  (CDSK(1,M), N, XADSK(1,1,M), SM2)
  101 continue
C
C---- Compute XJNU
      do 102 I = 1,N
        XNUM = RR(I)+SM1(I)
        XDEN = ONE  -SM2(I)
        call DIVIDE (XNUM, XDEN, RAT)
        XJNU(I) = max(RAT,ZERO)
  102 continue
C
      if(DUMP) then
        call BLEDA  (N, XJNUO, SM1, SM2, XJNU, ITER)
      end if
C     !END
      call BYE ('RUGILA')
C
      return
      end
