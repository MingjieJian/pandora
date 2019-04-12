      subroutine ROTINGA
     $(N,NL,NSL,BDIJ,GMI,RKI,RLI,CKI,CQUI,BDX,GVL,KDGV,BDI,SQS,STM,BRJ,
     $ XNUM,XDEN)
C
C     Rudolf Loeser, 1988 Jan 05
C---- Computes "b from b-ratios" using the continuum equation.
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIJ, BDX, BRJ, CKI, CQUI, GMI, GVL, RKI, RLI, SQS,
     $       STM, XDEN, XNUM
      integer I, J, KDGV, N, NL, NSL
      logical DUMP
C     !DASH
      external WEDGE, SICKLE, ARRADD, ARRDIV, SCYTHE, MUSSEL, MASHED,
     $         HEM, TULBE, HI, BYE
C
C               CQUI(N,NSL), GVL(N,NL), RKI(N,NSL), RLI(N,NSL), STM(N),
      dimension CQUI(*),     GVL(*),    RKI(*),     RLI(*),     STM(*),
C
C               CKI(N,NSL), XNUM(N), BDX(N,NSL), BDIJ(N,NL), BDI(N,NL),
     $          CKI(*),     XNUM(*), BDX(*),     BDIJ(N,*),  BDI(N,*),
C
C               GMI(N,NSL), SQS(N), XDEN(N), BRJ(N,NL)
     $          GMI(*),     SQS(*), XDEN(*), BRJ(*)
C
      call HI ('ROTINGA')
C     !BEG
      call TULBE      ('ROTINGA', DUMP)
C---- Compute Numerator
      call WEDGE      (N, NL, NSL, GMI, RLI, RKI, CKI, BDX, STM)
      call ARRADD     (SQS, STM, XNUM, N)
      if(DUMP) then
        call SICKLE   (N, SQS, STM, XNUM)
      end if
C
C---- For all levels
      do 102 J = 1,NL
C---    Get b-ratio w.r.t. J
        call HEM      (J, N, NL, BDIJ, BRJ)
C----   For all depths
        do 101 I = 1,N
          call MUSSEL (I, N, NL, CQUI, GMI, GVL, KDGV, BRJ, XDEN(I))
  101   continue
        call ARRDIV   (XNUM, XDEN, BDI(1,J), N)
        if(DUMP) then
          call SCYTHE (J, N, NL, XDEN, BDI(1,J), BRJ)
        end if
  102 continue
      if(DUMP) then
        call MASHED   ('ROTINGA')
      end if
C     !END
      call BYE ('ROTINGA')
C
      return
      end
