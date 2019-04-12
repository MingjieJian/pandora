      subroutine AZPRIN
     $(X,W,IW,IU,IL,KLIN,N,XND,BDI,P,SET,XX,STIM,EQUL,SMTH,STMA,STMB,
     $ STMAS,STMBS,LU)
C
C     Rudolf Loeser, 2004 Oct 04
C---- Computes STIM for GTN.
C     (This is version 2 of AZPRIN.)
C     !DASH
      save
C     !DASH
      real*8 BDI, DELTA, ONE, P, PLU, RAT, SET, STIM, STMA, STMAS, STMB,
     $       STMBS, W, X, XND, XX
      integer I, IL, INDX, IQGSM, IQGTS, IU, IW, JUL, K, KEQU, KLIN,
     $        KNTA, KNTB, LU, N
      logical EQUL, OKA, OKB, SMTH
      character LABEL*30, TYPE*3
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(275),IQGTS)
      equivalence (IQQ(332),IQGSM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external INDXUL, DIVIDE, MOVE1, SMOOTH, ANAKLID, ZERO1, ARISOD,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               SET(N,MUL), STMBS(N), STMB(N), STMA(N), STIM(N), XX(N),
      dimension SET(N,*),   STMBS(*), STMB(*), STMA(*), STIM(*), XX(*),
C
C               STMAS(N), BDI(N,NL), XND(N,NL), P(NSL)
     $          STMAS(*), BDI(N,*),  XND(N,*),  P(*)
C
      data TYPE,INDX,DELTA /'lin', 0, 1.D-8/
C
      call HI ('AZPRIN')
C     !BEG
      if(KLIN.ne.1) then
        call ZERO1 (STIM, N)
      else
C
        write (LABEL,100) IU,IL
  100   format(6X,' for GTN(',I2,'/',I2,')')
C     !EJECT
C----   Compute STIM-A
        call DIVIDE    (P(IL), P(IU), PLU)
        do 101 I = 1,N
          call DIVIDE  (XND(I,IU), XND(I,IL), RAT)
          STMA(I) = ONE-PLU*RAT
  101   continue
        call MOVE1     (STMA, N, STMAS)
        if(IQGSM.gt.0) then
          LABEL(1:5) = 'STIMA'
          call SMOOTH  (XX, STMAS, N, TYPE, LABEL, INDX, W, IW, KNTA,
     $                  OKA)
        end if
C
C----   Compute STIM-B
        call INDXUL    (IU, IL, JUL)
        do 102 I = 1,N
          STMB(I) = ONE-SET(I,JUL)
  102   continue
        call MOVE1     (STMB, N, STMBS)
        if(IQGSM.gt.0) then
          LABEL(1:5) = 'STIMB'
          call SMOOTH  (XX, STMBS, N, TYPE, LABEL, INDX, W, IW, KNTB,
     $                  OKB)
        end if
C
C----   Select
        call ARISOD    (STMA, N, STMB, N, DELTA, KEQU)
        EQUL = KEQU.gt.0
        if(IQGTS.gt.0) then
          K = 2
          call MOVE1   (STMBS, N, STIM)
          SMTH = KNTB.gt.0
        else
          K = 1
          call MOVE1   (STMAS, N, STIM)
          SMTH = KNTA.gt.0
        end if
C
        if(LU.gt.0) then
C----     Print
          call ANAKLID (LU, IU, IL, K, N, P, SET(1,JUL), XND, BDI,
     $                  STMA, STMB, STMAS, STMBS)
        end if
      end if
C     !END
      call BYE ('AZPRIN')
C
      return
      end
