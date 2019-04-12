      subroutine BUMP
     $(KOPAC,C,XNUM)
C
C     Rudolf Loeser, 1974 Dec 05
C---- Computes numerator of Absorption Source Function.
C     !DASH
      save
C     !DASH
      real*8 C, XNUM, ZERO
      integer KNT, KOPAC, LIST
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external VICIA, HI, BYE
C
C               C(Nopac), KOPAC(Nopac)
      dimension C(*),     KOPAC(*)
C
      parameter (KNT=33)
      dimension LIST(KNT)
C
      data LIST /  1,  2,  5,  6,  7,  8,  9, 10, 11, 12, 13, 15,
     $            17, 18, 19, 20, 21, 22, 24, 27, 31, 33, 34, 35,
     $            37, 38, 39, 40, 41, 42, 43, 44, 45/
C
      call HI ('BUMP')
C     !BEG
C---- Initialize
      XNUM = ZERO
C
C---- Add designated contributors
      call VICIA (C, KOPAC, LIST, KNT, XNUM)
C     !END
      call BYE ('BUMP')
C
      return
      end
