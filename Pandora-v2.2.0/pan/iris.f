      subroutine IRIS
     $(IU,IL,NL,AIJ,XNU,DEFAULT)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Computes default value of CRD,
C     the radiative line broadening parameter.
C     (This is version 4 of IRIS.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, DEFAULT, FAC, SUMIL, SUMIU, XNU
      integer IL, IU, NL
C     !DASH
      external EDNA, ZIRA, HI, BYE
C
C               AIJ(NL,NL), XNU(NSL)
      dimension AIJ(*),     XNU(*)
C
      call HI ('IRIS')
C     !BEG
      call EDNA (XNU, IU, IL, FAC)
      call ZIRA (AIJ, IU, NL, SUMIU)
      call ZIRA (AIJ, IL, NL, SUMIL)
C
      DEFAULT = FAC*(SUMIU+SUMIL)
C     !END
      call BYE ('IRIS')
C
      return
      end
