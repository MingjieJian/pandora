      subroutine AWN
     $(DEE,HE1,HE2K,HEND,B1,B2)
C
C     Rudolf Loeser, 1998 Apr 06
C---- Computes B1 and B2 for Helium "f's," for Special N1 calculation.
C     (This is version 5 of AWN.)
C     !DASH
      save
C     !DASH
      real*8 ALPHA, B1, B2, DEE, GAMMA, HE1, HE2K, HEND, OMGAM, ONE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               DEE(4,5)
      dimension DEE(4,5)
C
      call HI ('AWN')
C     !BEG
      call DIVIDE (HE1 ,HEND,ALPHA)
      call DIVIDE (HE2K,HEND,GAMMA)
      OMGAM = ONE-GAMMA
C
      B1 = DEE(3,3)*OMGAM-DEE(3,4)*ALPHA
      B2 = DEE(4,3)*OMGAM-DEE(4,4)*ALPHA
C     !END
      call BYE ('AWN')
C
      return
      end
