      subroutine GETPIJ
     $(KDGV,AL,RKI,RLI,CKI,CQSI,SQS,SPKL,SLT,PIS,GMI,GVL,PIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Recomputes and prints bound-free-bound transition rates.
C     (This is version 2 of SETPIJ.)
C     !DASH
      save
C     !DASH
      real*8 AL, CKI, CQSI, GMI, GVL, PIJ, PIS, RKI, RLI, SLT, SPKL,
     $       SQS
      integer I, KDGV, N, NL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C     !DASH
      external SACATON, SMILO, PEST, PULUGA, HI, BYE
C
C               GVL(N,NL), AL(NL), RKI(N,NSL), PIS(N,NL), PIJ(N,NL,NL),
      dimension GVL(*),    AL(*),  RKI(*),     PIS(*),    PIJ(*),
C
C               CKI(N,NSL), CQSI(N,NSL), GMI(N,NSL), SQS(N), SPKL(N),
     $          CKI(*),     CQSI(*),     GMI(*),     SQS(*), SPKL(*),
C
C               SLT(N), RLI(N,NSL)
     $          SLT(*), RLI(*)
C
      call HI ('GETPIJ')
C     !BEG
C---- Compute "supplementary levels term" SLT
      call SACATON (N, NL, NSL, GMI, RLI, RKI, SPKL, SLT)
C---- Compute PIS
      call SMILO   (N, NL, NSL, RKI, CKI, SQS, SLT, GVL, KDGV, PIS)
C
C---- Compute PIJ
      do 100 I = 1,N
        call PEST  (I, N, NL, RKI, CKI, CQSI, SQS, SLT, GVL, KDGV, PIS,
     $              AL, PIJ)
  100 continue
C
C---- Dump (if needed)
      call PULUGA  (N, KDGV, SQS, SLT)
C     !END
      call BYE ('GETPIJ')
C
      return
      end
