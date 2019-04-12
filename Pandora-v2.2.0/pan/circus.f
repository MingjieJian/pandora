      subroutine CIRCUS
     $(X,IX,N,NSL,NTE,TER,CII,TE,XNU,XNUC,NPQ,LRQ,NLE,XNE,XNC,CMCI,
     $ CACI,CKIAD,CKI,FCK,CHKI,NO)
C
C     Rudolf Loeser, 1978 Jan 26
C---- Computes collisional ionization rate, for SETTUP.
C     !DASH
      save
C     !DASH
      real*8 CACI, CHKI, CII, CKI, CKIAD, CMCI, FCK, TE, TER, X, XNC,
     $       XNE, XNU, XNUC
      integer I, IX, J, KCHKI, KFELE, LRQ, N, NLE, NNSL, NO, NPQ, NSL,
     $        NTE
      logical DUMP, KILROY
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(70),KFELE)
      equivalence (LEST(80),KCHKI)
C     !DASH
C     !EJECT
      external ADHEMAR, NANCY, COOT, SERAI, ARRADD, MASHED, HI, BYE
C
      dimension X(*), IX(*)
C
C               CKI(N,NSL), FCK(N), XNE(N), XNU(NSL), TER(NTE), XNC(N),
      dimension CKI(N,*),   FCK(*), XNE(*), XNU(*),   TER(*),   XNC(*),
C
C               CII(NTE,NSL), CHKI(N,NSL), CMCI(NSL), TE(N), XNUC(NSL),
     $          CII(*),       CHKI(*),     CMCI(*),   TE(*), XNUC(*),
C
C               CACI(NSL), NPQ(NSL), LRQ(NSL), NLE(NSL), CKIAD(N,NSL)
     $          CACI(*),   NPQ(*),   LRQ(*),   NLE(*),   CKIAD(*)
C
      call HI ('CIRCUS')
C     !BEG
      NNSL = N*NSL
C
      KILROY = .true.
C---- Normal calculation of collisions with electrons
      do 101 J = 1,NSL
        do 100 I = 1,N
          call ADHEMAR (KILROY, 'CIRCUS', I, J, DUMP)
          call NANCY   (J, TE(I), XNC(I), CMCI, CACI, XNE(I), XNU,
     $                  XNUC, NPQ, LRQ, NLE, TER, NTE, CII, DUMP,
     $                  CKI(I,J))
  100   continue
  101 continue
      if(.not.KILROY) then
        call MASHED    ('CIRCUS')
      end if
C
      if(KFELE.eq.1) then
C----   Replace level-1 values by results from "fast electrons"
C       calculation
        call COOT      (N, NSL, NO, FCK, CKI)
      end if
C
C---- Add "additional" input amount
      call ARRADD      (CKI, CKIAD, CKI, NNSL)
C
      if(KCHKI.gt.0) then
C----   Analyze ionization
        call SERAI     (X, IX, TE, XNE, XNC, XNU, XNUC, CKI, CHKI, NO)
C----   Add terms due to collisions with Hydrogen atoms
        call ARRADD    (CKI, CHKI, CKI, NNSL)
      end if
C     !END
      call BYE ('CIRCUS')
C
      return
      end
