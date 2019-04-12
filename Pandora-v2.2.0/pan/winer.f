      subroutine WINER
     $(HND,FNH)
C
C     Rudolf Loeser, 2000 Mar 09
C---- Computes a density-dependent term.
C     !DASH
      save
C     !DASH
      real*8 FNH, HALF, HL, HND, ONE, SEVEN, ZERO, ZX1, ZX2
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 8),SEVEN )
C     !DASH
      external HI, BYE
C
      data ZX1,ZX2 /1.D12, 1.D14/
C
      call HI ('WINER')
C     !BEG
      if(HND.le.ZX1) then
        FNH = ONE
      else if(HND.ge.ZX2) then
        FNH = ZERO
      else
        HL  = log10(HND)
        FNH = SEVEN-HALF*HL
      end if
C     !END
      call BYE ('WINER')
C
      return
      end
