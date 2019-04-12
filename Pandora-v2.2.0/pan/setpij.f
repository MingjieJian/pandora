      subroutine SETPIJ
     $(NO,AL,RKI,RLI,CKI,CQSI,SQS,SPKL,SLT,PIS,GMI,GVL,PIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Recomputes and prints bound-free-bound transition rates.
C     (This is version 2 of SETPIJ.)
C     !DASH
      save
C     !DASH
      real*8 AL, CKI, CQSI, GMI, GVL, PIJ, PIS, RKI, RLI, SLT, SPKL,
     $       SQS
      integer KDGV, N, NL, NO, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(39),KDGV )
C     !DASH
      external GETPIJ, PETPIJ, HI, BYE
C
C               GVL(N,NL), AL(NL), RKI(N,NSL), PIS(N,NL), PIJ(N,NL,NL),
      dimension GVL(*),    AL(*),  RKI(*),     PIS(*),    PIJ(*),
C
C               CQSI(N,NSL), GMI(N,NSL), RLI(N,NSL), SQS(N), SPKL(N),
     $          CQSI(*),     GMI(*),     RLI(*),     SQS(*), SPKL(*),
C
C               SLT(N), CKI(N,NSL)
     $          SLT(*), CKI(*)
C
      call HI ('SETPIJ')
C     !BEG
C---- Compute
      call GETPIJ (KDGV,AL,RKI,RLI,CKI,CQSI,SQS,SPKL,SLT,PIS,GMI,GVL,
     $             PIJ)
C---- Print
      call PETPIJ (NO,N,NL,PIJ,NSL,PIS)
C     !END
      call BYE ('SETPIJ')
C
      return
      end
