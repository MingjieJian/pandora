      subroutine COLCHIS
     $(XINT,NRAD,NLAM,XLAM,PLAM,NP,SINT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Extracts subset of XINT, for AMASIA.
C     !DASH
      save
C     !DASH
      real*8 PLAM, SINT, XINT, XLAM
      integer I, NLAM, NP, NRAD
C     !DASH
      external FERE, HI, BYE
C
C               XINT(NRAD,NLAM), SINT(NRAD,NP), PLAM(NP), XLAM(NLAM)
      dimension XINT(NRAD,*),    SINT(NRAD,*),  PLAM(*),  XLAM(*)
C
      call HI ('COLCHIS')
C     !BEG
      do 100 I = 1,NRAD
        call FERE (XLAM,1,XINT(I,1),NRAD,NLAM,
     $             PLAM,1,SINT(I,1),NRAD,  NP,   2,1)
  100 continue
C     !END
      call BYE ('COLCHIS')
C
      return
      end
