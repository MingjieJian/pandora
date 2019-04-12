      subroutine FOTOR
     $(N,LG,CI,XA,WN,RR,XJNUO,SUM1,SUM2,XJNU,ITER,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes new XJNU from previous XJNU, for angle-dependent
C     Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CI, ONE, RR, SUM1, SUM2, WN, XA, XDEN, XJNU, XJNUO, XNUM,
     $       ZERO
      integer I, ITER, K, LG, N
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
      external ZERO1, ODEON, THELA, BLEDA, BOUNDLO, DIVIDE, HI, BYE
C
C               CI(N,LG), XA(N,N,LG), WN(N,N,LG), XJNU(N), XJNUO(N),
      dimension CI(N,*),  XA(N,N,*),  WN(N,N,*),  XJNU(*), XJNUO(*),
C
C               SUM1(N), SUM2(N), RR(N)
     $          SUM1(*), SUM2(*), RR(*)
C
      call HI ('FOTOR')
C     !BEG
C---- Compute SUM1 and SUM2
      call ZERO1    (SUM1, N)
      call ZERO1    (SUM2, N)
      do 100 K = 1,LG
        call ODEON  (CI(1,K), N, WN(1,1,K), XA(1,1,K), XJNUO, SUM1)
        call THELA  (CI(1,K), N,            XA(1,1,K),        SUM2)
  100 continue
C
C---- Compute XJNU
      do 101 I = 1,N
        XNUM = RR(I)+SUM1(I)
        XDEN = ONE-SUM2(I)
        call DIVIDE (XNUM, XDEN, XJNU(I))
  101 continue
      call BOUNDLO  (N, XJNU, ZERO)
C
      if(DUMP) then
        call BLEDA  (N, XJNUO, SUM1, SUM2, XJNU, ITER)
      end if
C     !END
      call BYE ('FOTOR')
C
      return
      end
