      subroutine EXPOUND
     $(N,NL,NSL,GMI,CKI,RKI,RLI,CQUI,CQSI,PIJ,CIJ,CHKI,CHIJ)
C
C     Rudolf Loeser, 1989 Jul 05
C---- Checksums, for SETTUP.
C     (This is version 2 of EXPOUND.)
C     !DASH
      save
C     !DASH
      real*8 CHIJ, CHKI, CIJ, CKI, CQSI, CQUI, GMI, PIJ, RKI, RLI
      integer IOVER, KCHIJ, KCHKI, N, NL, NN2, NNL, NSL
      character TIT*12
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST(71),KCHIJ)
      equivalence (LEST(80),KCHKI)
C     !DASH
C     !EJECT
      external CHECKER, HI, BYE
C
C               CQUI(N,NSL), CQSI(N,NSL), PIJ(N,NL,NL), CHIJ(N,NL,NL),
      dimension CQUI(*),     CQSI(*),     PIJ(*),       CHIJ(*),
C
C               GMI(N,NSL), CHKI(N,NSL), RLI(N,NSL), CIJ(N,NL,NL),
     $          GMI(*),     CHKI(*),     RLI(*),     CIJ(*),
C
C               CKI(N,NSL), RKI(N,NSL)
     $          CKI(*),     RKI(*)
C
      call HI ('EXPOUND')
C     !BEG
      NNL = N*NSL
      NN2 = N*(NL**2)
C
      write (TIT,100) IOVER
  100 format(', IOVER =',I3)
C
      call CHECKER   (GMI,  1, NNL,  'GMI'//TIT)
      call CHECKER   (CKI,  1, NNL,  'CKI'//TIT)
      call CHECKER   (RKI,  1, NNL,  'RKI'//TIT)
      call CHECKER   (RLI,  1, NNL,  'RLI'//TIT)
      call CHECKER   (CQUI, 1, NNL,  'QUI'//TIT)
      call CHECKER   (CQSI, 1, NNL,  'QSI'//TIT)
      call CHECKER   (PIJ,  1, NN2,  'PIJ'//TIT)
      call CHECKER   (CIJ,  1, NN2,  'CIJ'//TIT)
      if(KCHKI.gt.0) then
        call CHECKER (CHKI, 1, NNL, 'CHKI'//TIT)
      end if
      if(KCHIJ.gt.0) then
        call CHECKER (CHIJ, 1, NN2, 'CHIJ'//TIT)
      end if
C     !END
      call BYE ('EXPOUND')
C
      return
      end
