      subroutine HEDRI
C
C     Rudolf Loeser, 1996 Jan 04
C---- Edits Element tables, to suppress entries whose Abundance=0.
C     (This is version 2 of HEDRI.)
C     !DASH
      save
C     !DASH
      real*8 ZERO
      integer I, J, K, L, LIM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !DASH
      external  HI, BYE
C     !EJECT
C
      call HI ('HEDRI')
C     !BEG
      K = 1
  100 continue
C
        do 101 J = K,NMT
          L = J
          if(ELABD(J).eq.ZERO) goto 102
  101   continue
        goto 104
  102   continue
C
        LIM = L+1
        do 103 I = (L+1),NMT
          ELSYM(I-1) = ELSYM(I)
          ELABD(I-1) = ELABD(I)
          ELCHI(I-1) = ELCHI(I)
          ELLU1(I-1) = ELLU1(I)
          ELLU2(I-1) = ELLU2(I)
          LATNO(I-1) = LATNO(I)
          ELABL(I-1) = ELABL(I)
          ELDEF(I-1) = ELDEF(I)
          LDEFR(I-1) = LDEFR(I)
  103   continue
C
        NMT = NMT-1
        K   = L
C
      if(K.le.NMT) goto 100
  104 continue
C     !END
      call BYE ('HEDRI')
C
      return
      end
