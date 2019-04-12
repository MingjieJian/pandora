      subroutine NICE
     $(N,NL,NCK,NT,CHECK,TAU,S,RHO,RHOWT,XND,RK,BD,XNE,Z,HND,TDST,XNK,
     $ A,PRAT,QHI,NO)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Supervises production of iterative summaries.
C     (This is version 2 of NICE.)
C     !DASH
      save
C     !DASH
      real*8 A, BD, CHECK, HND, PRAT, QHI, RHO, RHOWT, RK, S, TAU, TDST,
     $       XND, XNE, XNK, Z
      integer KIBI, N, NCK, NL, NO, NT
C     !DASH
      external THRUSH, LAZICA, JUNCO, MOLASES, HI, BYE
C
C               CHECK(N,NCK,NCKITR), TAU(N,NT,NTAITR), S(N,NT,NSSITR),
      dimension CHECK(*),            TAU(*),           S(*),
C
C               RHO(N,NT,NRHITR), RHOWT(N,NT,NRWITR), XND(N,NL,NNDITR),
     $          RHO(*),           RHOWT(*),           XND(*),
C
C               RK(N,NRKITR), BD(N,NBKITR), XNE(N,NNEITR), Z(N,NZZITR),
     $          RK(*),        BD(*),        XNE(*),        Z(*),
C
C               HND(N,NNHITR), TDST(N,NTDITR), A(N,max iters), PRAT(N),
     $          HND(*),        TDST(*),        A(*),           PRAT(*),
C
C               XNK(N,NNKITR), QHI(N,NT,NQHITR)
     $          XNK(*),        QHI(*)
C
C
      call HI ('NICE')
C     !BEG
C---- KIBI is the counter of lines in the "Iteration Trend Summary',
C     needed for MOLASES, below.
      KIBI = 0
C
C---- For: CHECK, XND, XNK, XNE, HND
      call THRUSH  (N, NL, NCK, CHECK, XND, XNK, XNE, HND, A, KIBI, NO,
     $              PRAT)
C---- For: TAU, RHO, RHOWT, S, CHI
      call LAZICA  (N, NT, TAU, RHO, RHOWT, S, QHI, A, KIBI, NO, PRAT)
C---- For: RK, BD, Z, TDST
      call JUNCO   (N, RK, BD, Z, TDST, KIBI, NO, PRAT)
C
C---- And now, print the "Iteration Trend Summary" as compiled in
C     TREND as a result of the preceding subroutine calls
C     ("TREND" is a buffer in labelled common SOLO).
      call MOLASES (NO, KIBI, N)
C     !END
      call BYE ('NICE')
C
      return
      end
