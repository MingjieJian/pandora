      subroutine CUTIE
     $(N,GMI,RKI,RLI,CKI,CQUI,CQSI)
C
C     Rudolf Loeser, 1974 Dec 26
C---- Computes ionization terms for a particular level.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, CQUI, GMI, RKI, RLI
      integer N
C     !DASH
      external ARRADD, ARRMUL, HI, BYE
C
C               GMI(N), RKI(N), RLI(N), CKI(N), CQUI(N), CQSI(N)
      dimension GMI(*), RKI(*), RLI(*), CKI(*), CQUI(*), CQSI(*)
C
      call HI ('CUTIE')
C     !BEG
      call ARRADD (RKI ,CKI,CQUI,N)
      call ARRMUL (CQUI,GMI,CQUI,N)
      call ARRADD (RLI ,CKI,CQSI,N)
      call ARRMUL (CQSI,GMI,CQSI,N)
C     !END
      call BYE ('CUTIE')
C
      return
      end
