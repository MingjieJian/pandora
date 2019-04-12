      subroutine GIZON
     $(IU,IL,NL,CRS,AIJ,XNU)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Computes default value of Hydrogen CRS,
C     the resonance line broadening parameter.
C     !DASH
      save
C     !DASH
      real*8 AIJ, CRS, DNU, FAC, TL, TU, XNU
      integer IL, IU, NL
C     !DASH
      external MADO, HI, BYE
C
C               AIJ(NL,NL), XNU(NSL)
      dimension AIJ(*),     XNU(*)
C
      data FAC /5.952D-14/
C
      call HI ('GIZON')
C     !BEG
      call MADO (IU,NL,AIJ,XNU,TU)
      call MADO (IL,NL,AIJ,XNU,TL)
      DNU = XNU(IU)-XNU(IL)
      CRS = (FAC/(DNU**2))*(TU+TL)
C     !END
      call BYE ('GIZON')
C
      return
      end
