      subroutine QUETZAL
     $(N,NL,NSL,GMI,RKI,RLI,CKI,TE,SA,HND,XNE,CQUI,CQSI,SQS,DRCT,NO)
C
C     Rudolf Loeser, 2001 May 31
C---- Computes and prints ionization terms, for SETTUP.
C     (This is version 2 of QUETZAL.)
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, CQUI, DRCT, GMI, HND, RKI, RLI, SA, SQS, TE,
     $       XNE
      integer N, NL, NO, NSL
C     !DASH
      external QUIT, WHEAT, HI, BYE
C
C               RKI(N,NSL), CQSI(N,NSL), GMI(N,NSL), CKI(N,NSL), TE(N),
      dimension RKI(*),     CQSI(*),     GMI(*),     CKI(*),     TE(*),
C
C               CQUI(N,NSL), RLI(N,NSL), HND(N), SQS(N), XNE(N), SA(N),
     $          CQUI(*),     RLI(*),     HND(*), SQS(*), XNE(*), SA(*),
C
C               DRCT(N)
     $          DRCT(*)
C
      call HI ('QUETZAL')
C     !BEG
      call QUIT    (N,NL,NSL,1,NSL,GMI,RKI,RLI,CKI,TE,SA,HND,XNE,CQUI,
     $              CQSI,SQS,DRCT)
      if(NO.gt.0) then
        call WHEAT (NO,N,NSL,CQUI,CQSI,SQS,DRCT)
      end if
C     !END
      call BYE ('QUETZAL')
C
      return
      end
