      subroutine HABREN
     $(MN1,W,IW,Z,A,B,C,D,E,XMUL,IMG,FO,ALFA,BETA,DABN)
C
C     Rudolf Loeser, 2000 Nov 15
C---- Normalizes alpha & beta for the simultaneous diffusion solution.
C     (This is version 2 of HABREN.)
C     !DASH
      save
C     !DASH
      real*8 A, ALFA, B, BETA, C, D, E, FO, ONE, W, XMUL, Z, ZERO
      integer I, IMG, INDX, IW, KERM, KMSS, KODE, MN1, MODE, NERM,
     $        jummy
      logical DABN, lummy
      character TITLE*37, TYPE*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, EDITH, SMOOTH, DIVIDE, ARRMUL, HI, BYE
C
      dimension W(*), IW(*)
C
C               IMG(N), ALFA(N), BETA(N), A(N), B(N), C(N), D(N), Z(N),
      dimension IMG(*), ALFA(*), BETA(*), A(*), B(*), C(*), D(*), Z(*),
C
C               XMUL(N), E(N), FO(N)
     $          XMUL(*), E(*), FO(*)
C
      data TYPE,INDX /'lin', 0/
      data TITLE /'Term c for alpha & beta normalization'/
      data KODE,MODE /2, 1/
      data KERM,NERM,KMSS /0, 100000, 0/
C     !EJECT
C
      call HI ('HABREN')
C     !BEG
      DABN = .true.
C
      call MOVE1    (ALFA, MN1, A)
      call MOVE1    (BETA, MN1, B)
      do 100 I = 1,MN1
        C(I) = ONE-A(I)-B(I)
  100 continue
C
      call MOVE1    (C, MN1, D)
      call EDITH    (D, MN1, ZERO, KODE, MODE, KMSS, TITLE, IMG, FO,
     $               KERM, NERM, lummy)
C
      call MOVE1    (D, MN1, E)
      call SMOOTH   (Z, E, MN1, TYPE, TITLE, INDX, W, IW, jummy, lummy)
C
      do 101 I = 1,MN1
        call DIVIDE ((ONE-E(I)), (A(I)+B(I)), XMUL(I))
  101 continue
C
      call ARRMUL   (A, XMUL, ALFA, MN1)
      call ARRMUL   (B, XMUL, BETA, MN1)
C     !END
      call BYE ('HABREN')
C
      return
      end
