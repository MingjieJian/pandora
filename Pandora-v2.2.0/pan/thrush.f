      subroutine THRUSH
     $(N,NL,NCK,CHECK,XND,XNK,XNE,HND,ARR,KIBI,NO,PRAT)
C
C     Rudolf Loeser, 1984 Jan 25
C---- Supervises some iterative summaries.
C     !DASH
      save
C     !DASH
      real*8 ARR, CHECK, HND, PRAT, XND, XNE, XNK
      integer INDGR, INPOMT, INPUSE, J, KIBI, N, NCK, NCKITR, NL,
     $        NNDITR, NNEITR, NNHITR, NNKITR, NO
      logical TRND
      character TITLE*15
C     !COM
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
      equivalence (NITS( 2),NCKITR)
      equivalence (NITS( 5),NNDITR)
      equivalence (NITS(14),NNKITR)
      equivalence (NITS( 8),NNEITR)
      equivalence (NITS(12),NNHITR)
C     !DASH
      external RUB, CROW, HI, BYE
C
C               CHECK(N,NCK,NCKITR), XND(N,NL,NNDITR), XNK(N,NNKITR),
      dimension CHECK(*),            XND(*),           XNK(*),
C
C               PRAT(N), XNE(N,NNEITR), HND(N,NNHITR), ARR(N,maxiters)
     $          PRAT(*), XNE(*),        HND(*),        ARR(*)
C
      data TRND   /.true./
      data INDGR  /1/
      data INPUSE /1/
      data INPOMT /0/
C     !EJECT
C
      call HI ('THRUSH')
C     !BEG
      if((NCK.gt.0).and.(NCKITR.gt.1)) then
        do 101 J = 1,NCK
          call RUB  (N, NCK, NCKITR, J, CHECK, ARR)
          write (TITLE,100) (J+2)
  100     format('CHECK ',I2,7X)
          call CROW (N, NCKITR, INPOMT, ARR, TITLE, NO, KIBI, INDGR,
     $               TRND, PRAT)
  101   continue
      end if
C
      if(NNDITR.gt.1) then
        do 103 J = 1,NL
          call RUB  (N, NL, NNDITR, J, XND, ARR)
          write (TITLE,102) J
  102     format('ND ',I2,10X)
          call CROW (N, NNDITR, INPOMT, ARR, TITLE, NO, KIBI, INDGR,
     $               TRND, PRAT)
  103   continue
      end if
C
      if(NNKITR.gt.1) then
        TITLE = 'NK             '
        call CROW   (N, NNKITR, INPOMT, XNK, TITLE, NO, KIBI, INDGR,
     $               TRND, PRAT)
      end if
C
      if(NNEITR.gt.1) then
        TITLE = 'NE             '
        call CROW   (N, NNEITR, INPUSE, XNE, TITLE, NO, KIBI, INDGR,
     $               TRND, PRAT)
      end if
C
      if(NNHITR.gt.1) then
        TITLE = 'NH             '
        call CROW   (N, NNHITR, INPUSE, HND, TITLE, NO, KIBI, INDGR,
     $               TRND, PRAT)
      end if
C     !END
      call BYE ('THRUSH')
C
      return
      end
