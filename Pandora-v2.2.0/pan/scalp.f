      subroutine SCALP
     $(AD,A,CRAT)
C
C     Rudolf Loeser, 1969 Jun 20
C---- Computes a controlled value for the ratio AD/A.
C     !DASH
      save
C     !DASH
      real*8 A, AD, CRAT, ONE, Z1, Z2, ZERO
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
      external  HI, BYE
      intrinsic sign, abs
C
      data Z1,Z2 /9.99999D0, 2.22222D0/
C
      call HI ('SCALP')
C     !BEG
      if(A.eq.ZERO) then
        if(AD.eq.ZERO) then
          CRAT = ONE
        else
          CRAT = Z1
        end if
      else
        CRAT = AD/A
        if(abs(CRAT).gt.Z2) then
          CRAT = sign(Z2,CRAT)
        end if
      end if
C     !END
      call BYE ('SCALP')
C
      return
      end
