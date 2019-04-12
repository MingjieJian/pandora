      subroutine TANYA
     $(STEP,K,N,TE,XNE,D,L,KNW,ARRK,FKUR)
C
C     Rudolf Loeser, 1980 Dec 19
C---- Gets actual opacities, as functions of depth, by interpolation,
C     from the raw Statistical Line Opacity data array.
C     (This is version 2 of TANYA.)
C     !DASH
      save
C     !DASH
      real*8 ARRK, CAP, D, FKUR, OPAC, OTEA, OTEB, STEP, TE, TELM, TEN,
     $       TET, XNE, XNELM, XNET
      integer I, J, K, KNW, L, LEQ, M, MODE, N, NE, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
      external NIRVANA, LINT, HI, BYE
C
      parameter (NT=11, NE=9)
C
C               STEP(NT,NE,10), ARRK(KNW,N), FKUR(KNW), XNE(N), TE(N),
      dimension STEP(NT,NE,*),  ARRK(KNW,*), FKUR(*),   XNE(*), TE(*),
C
C               D(N)
     $          D(*)
C
      dimension TET(NT), XNET(NE)
C
      data TET  /3.60D0, 3.64D0, 3.68D0, 3.72D0, 3.76D0, 3.80D0, 3.84D0,
     $           3.88D0, 3.92D0, 3.96D0, 4.00D0/
      data XNET /9.00D0, 1.00D1, 1.10D1, 1.20D1, 1.30D1, 1.40D1, 1.50D1,
     $           1.60D1, 1.70D1/
C
      data MODE /1/
C     !EJECT
C
      call HI ('TANYA')
C     !BEG
      do 110 M = 1,N
        TELM  = log10(TE(M))
        XNELM = log10(XNE(M))
        call NIRVANA (TELM, XNELM, TET, NT, XNET, NE, I, J, LEQ)
C
        goto (101,102,103,104,105,106,107,108,109), LEQ
  101   continue
          call LINT  (XNET(J), STEP(I,J,K),   XNET(J+1),
     $                STEP(I,  J+1,K), MODE, XNELM, OTEA)
          call LINT  (XNET(J), STEP(I+1,J,K), XNET(J+1),
     $                STEP(I+1,J+1,K), MODE, XNELM, OTEB)
          call LINT  (TET(I),  OTEA,          TET(I+1),
     $                OTEB,            MODE, TELM,  OPAC)
          goto 100
  102   continue
          call LINT  (XNET(J), STEP(NT,J,K), XNET(J+1),
     $                STEP(NT,J+1,K), MODE, XNELM, OPAC)
          goto 100
  103   continue
          call LINT  (XNET(J), STEP(1, J,K), XNET(J+1),
     $                STEP(1, J+1,K), MODE, XNELM, OPAC)
          goto 100
  104   continue
          call LINT  (TET(I),  STEP(I,NE,K), TET(I+1),
     $                STEP(I+1,NE,K), MODE, TELM,  OPAC)
          goto 100
  105   continue
          OPAC = STEP(NT,NE,K)
          goto 100
  106   continue
          OPAC = STEP(1, NE,K)
          goto 100
  107   continue
          call LINT  (TET(I),  STEP(I,1, K), TET(I+1),
     $                STEP(I+1,1, K), MODE, TELM,  OPAC)
          goto 100
  108   continue
          OPAC = STEP(NT,1,K)
          goto 100
  109   continue
          OPAC = STEP(1, 1,K)
          goto 100
  100   continue
C
        CAP       = TEN**OPAC
        ARRK(L,M) = D(M)*FKUR(L)*CAP
  110 continue
C     !END
      call BYE ('TANYA')
C
      return
      end
