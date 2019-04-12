      subroutine GAUNT
     $(TE,XLM,G)
C
C     Rudolf Loeser, 1971 Jan 14
C---- Computes Hydrogen free-free Gaunt factors, for HUFF.
C     !DASH
      save
C     !DASH
      real*8 C1, C10, C2, C3, C4, C5, C6, C7, C8, C9, G, ONE, T, TE,
     $       THREE, XLM, XLML, Z
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external  HI, BYE
      intrinsic max, min
C
      data      C1, C2, C3, C4  /1.D3, 2.D4, 4.D3, 1.32D-1/
      data      C5, C6, C7, C8  /1.62D-1, 1.58D4, 1.4D-1, 3.53D-3/
      data      C9, C10         /5.D3, 1.08D4/
C
      call HI ('GAUNT')
C     !BEG
      G = ONE
C
      if(XLM.gt.C1) then
        XLML = log10(XLM)
        Z    = XLML-THREE
        T    = max(min(TE,C2),C3)
C
        G = G+Z*((C4*Z-C5)*(C6-T)+(C7*Z-C8)*(T-C9))/C10
      end if
C     !END
      call BYE ('GAUNT')
C
      return
      end
