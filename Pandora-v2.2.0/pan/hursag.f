      subroutine HURSAG
     $(N,WHY,EFF,RHEAB)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Computes RHEAB for THALIA.
C     !DASH
      save
C     !DASH
      real*8 EFF, EMY, ONE, RHEAB, WHY
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LIMEXP, HI, BYE
C
C               WHY(N), EFF(N), RHEAB(N)
      dimension WHY(*), EFF(*), RHEAB(*)
C
      call HI ('HURSAG')
C     !BEG
      do 100 I = 1,N
        call LIMEXP ((-WHY(I)),EMY)
        RHEAB(I) = EMY*(ONE-EFF(I))
  100 continue
C     !END
      call BYE ('HURSAG')
C
      return
      end
