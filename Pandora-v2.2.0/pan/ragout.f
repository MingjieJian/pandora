      subroutine RAGOUT
     $(I,N,JS,NL,NSL,TE,SA,GM,RL,CK,HND,XNE,TQS,DRCT)
C
C     Rudolf Loeser, 2000 Mar 02
C---- Computes Recombination Terms.
C     (This is version 3 of RAGOUT.)
C     !DASH
      save
C     !DASH
      real*8 CK, DRCT, GM, HND, ONE, RL, SA, TE, TQS, XNE
      integer I, JS, N, NL, NSL
C     !COM
C---- RAGU        as of 2000 Mar 02
      real*8      RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,RADRC,
     $            RAWRR,RADEP,RAH,RATSQR,RATSQC,RASQS,RAX
      common      /RAGU/ RASAHA,RASQR,RASQRA,RASQC,RASQCA,RARRC,RACRR,
     $                   RADRC,RAWRR,RADEP,RATSQR,RATSQC,RASQS,RAH,RAX
C     Intermediate results of recombination calculation.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external AMANIT, AGARIC, TENERA, ERWIN, GILL, HI, BYE
C
C               TE(N), HND(N), XNE(N), CK(N,NSL), GM(N,NSL), RL(N,NSL),
      dimension TE(*), HND(*), XNE(*), CK(*),     GM(*),     RL(*),
C
C               SA(N)
     $          SA(*)
C
      call HI ('RAGOUT')
C     !BEG
      call AMANIT (I,N,JS    ,NL ,GM,RL,CK,RASQR ,RASQC )
      call AMANIT (I,N,(NL+1),NSL,GM,RL,CK,RASQRA,RASQCA)
      call AGARIC (TE(I),RACRR)
      call TENERA (TE(I),RAH)
      call ERWIN  (HND(I),XNE(I),RADEP)
      call GILL   (I,RAWRR)
C
      RASAHA = SA(I)
      RATSQR = RASQR+RASQRA
      RATSQC = RASQC+RASQCA
      RASQS  = RASQR+RASQC
      RADRC  = RAH*RADEP
      RARRC  = RATSQR*RASAHA
      RAX    = (ONE-RAWRR)*RATSQR+RAWRR*(RACRR/RASAHA)
      DRCT   = RADRC/RASAHA
      TQS    = RAX+DRCT+RATSQC
C     !END
      call BYE ('RAGOUT')
C
      return
      end
