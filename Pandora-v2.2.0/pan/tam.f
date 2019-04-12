      subroutine TAM
     $(TN,POPN,BD,FAC,T)
C
C     Rudolf Loeser, 1978 Oct 06
C---- Computes a term for EVAN.
C     (This is version 2 of TAM.)
C     !DASH
      save
C     !DASH
      real*8 BD, FAC, ONE, POPN, RAT, T, TN
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
      call HI ('TAM')
C     !BEG
      call DIVIDE (TN,BD,RAT)
      T = POPN*(ONE-RAT)*FAC
C     !END
      call BYE ('TAM')
C
      return
      end
