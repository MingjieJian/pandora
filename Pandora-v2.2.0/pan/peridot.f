      subroutine PERIDOT
     $(KOPAC,C,SCAT)
C
C     Rudolf Loeser, 1974 Dec 04
C---- Computes sum of scattering contributions.
C     !DASH
      save
C     !DASH
      real*8 C, SCAT, ZERO
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
      parameter (KNT=11)
      dimension LIST(KNT)
C
      data LIST /3, 4, 14, 16, 23, 25, 28, 29, 30, 32, 36/
C
      call HI ('PERIDOT')
C     !BEG
      SCAT = ZERO
      call VICIA (C, KOPAC, LIST, KNT, SCAT)
C     !END
      call BYE ('PERIDOT')
C
      return
      end
