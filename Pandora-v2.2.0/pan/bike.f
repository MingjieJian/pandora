      subroutine BIKE
     $(KAMB,MN1,HEND,RES,KBINN,KBOUT,ALFB,BETB,ALFA,BETA,EDIT,XN1,XNK,
     $ ALBE,GOLD,GNEW,GFIN,GNSM,GNED,Z,DUMP,KDAMP,SMGMMA,VEC,ARAT,BRAT,
     $ ANORM,BNORM,CNORM,DNORM,ENORM,IMG,FO,XMULT,DABN,W,IW)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Gets new N1 & NK, for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 ALBE, ALFA, ALFB, ANORM, ARAT, BETA, BETB, BFIN, BNORM,
     $       BRAT, CNORM, DNORM, EDIT, ENORM, FACT, FO, GFIN, GNED,
     $       GNEW, GNSM, GOLD, HEND, ONE, RES, VEC, W, XMULT, XN1, XNK,
     $       Z, ZERO
      integer I, IMG, IW, KAMB, KBINN, KBOUT, KDAMP, KERM, KMSS, KODE,
     $        MN1, MODE, NERM, NUM
      logical DABN, DUMP, SMGMMA, lummy
      character TA*30, TB*30, ZA*40, ZB*40
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
      external MOVE1, ARRMUL, BLEI, HABREN, EDITH, TROCAR, HI, BYE
C
      dimension W(*), IW(*)
C
C               HEND(N), ALFA(N), BETA(N), ALBE(N), XMULT(N), RES(2*N),
      dimension HEND(*), ALFA(*), BETA(*), ALBE(*), XMULT(*), RES(*),
C
C               ARAT(N), GFIN(N), GNSM(N), BRAT(N), ANORM(N), BNROM(N),
     $          ARAT(*), GFIN(*), GNSM(*), BRAT(*), ANORM(*), BNORM(*),
C
C               XNK(N), DNORM(N), CNORM(N), GOLD(N), GNEW(N), GNED(N),
     $          XNK(*), DNORM(*), CNORM(*), GOLD(*), GNEW(*), GNED(*),
C
C               Z(N), VEC(N), XN1(N), ENORM(N), IMG(N), FO(N)
     $          Z(*), VEC(*), XN1(*), ENORM(*), IMG(*), FO(*)
C
      data TA /'alpha from matrix solution'/
      data TB / 'beta from matrix solution'/
      data KODE,MODE      /2, 1/
      data KMSS,KERM,NERM /1, 0, 1000/
C
      data ZA /'alpha (edited) from matrix solution'/
      data ZB / 'beta (edited) from matrix solution'/
C     !EJECT
C
      call HI ('BIKE')
C     !BEG
      NUM = MN1-KBINN-KBOUT
      call MOVE1    (RES(    1), NUM, ALFA(1+KBINN))
      call MOVE1    (RES(NUM+1), NUM, BETA(1+KBINN))
      if(KBINN.eq.1) then
        ALFA(  1) = ALFB
        BETA(  1) = BETB
      else if(KBOUT.eq.1) then
        ALFA(MN1) = ALFB
        BETA(MN1) = BETB
      end if
C
      call EDITH    (ALFA, MN1, ZERO, KODE, MODE, KMSS, TA, IMG, FO,
     $               KERM, NERM, lummy)
      call EDITH    (BETA, MN1, ZERO, KODE, MODE, KMSS, TB, IMG, FO,
     $               KERM, NERM, lummy)
C
      call TROCAR   (KDAMP, ALFA, VEC, ARAT, MN1, ZA, W, IW, DUMP)
      call TROCAR   (KDAMP, BETA, VEC, BRAT, MN1, ZB, W, IW, DUMP)
C
      call HABREN   (MN1, W, IW, Z, ANORM, BNORM, CNORM, DNORM, ENORM,
     $               XMULT, IMG, FO, ALFA, BETA, DABN)
C
      if(KAMB.eq.2) then
        call ARRMUL (HEND, ALFA, XN1, MN1)
        call ARRMUL (HEND, BETA, XNK, MN1)
C
      else if(KAMB.eq.3) then
        do 100 I = 1,MN1
          ALBE(I) = ALFA(I)+BETA(I)
          GOLD(I) = XNK(I)/HEND(I)
          GNEW(I) = ONE-ALBE(I)
  100   continue
C
        call BLEI   (MN1, Z, GOLD, GNEW, GNED, GNSM, GFIN, W, IW,
     $               SMGMMA, 'gamma', DUMP)
C
        do 101 I = 1,MN1
          FACT   = (ONE-GFIN(I))/ALBE(I)
          BFIN   = FACT*BETA(I)
          XN1(I) = HEND(I)*BFIN
          XNK(I) = HEND(I)*GFIN(I)
  101   continue
      end if
C     !END
      call BYE ('BIKE')
C
      return
      end
