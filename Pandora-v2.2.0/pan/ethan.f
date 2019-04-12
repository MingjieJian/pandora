      subroutine ETHAN
     $(XINT,NRAD,NLAM,RADI,PADI,NP,SINT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Extracts subset of XINT, for GELIMER.
C     !DASH
      save
C     !DASH
      real*8 PADI, RADI, SINT, XINT
      integer J, NLAM, NP, NRAD
C     !DASH
      external FERE, HI, BYE
C
C               XINT(NRAD,NLAM), SINT(NLAM,NP), PADI(NP), RADI(NRAD)
      dimension XINT(NRAD,*),    SINT(NLAM,*),  PADI(*),  RADI(*)
C
      call HI ('ETHAN')
C     !BEG
      do 100 J = 1,NLAM
        call FERE (RADI,1, XINT(1,J),    1, NRAD,
     $             PADI,1, SINT(J,1), NLAM,   NP,  2,1)
  100 continue
C     !END
      call BYE ('ETHAN')
C
      return
      end
