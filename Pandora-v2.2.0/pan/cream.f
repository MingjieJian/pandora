      subroutine CREAM
     $(VECTOR,QNAME,I1,I2,LZA,ZAUX,Z,W)
C
C     Rudolf Loeser, 1972 Feb 01
C---- "Reads" vectors depending on depth.
C     !DASH
      save
C     !DASH
      real*8 VECTOR, W, Z, ZAUX
      integer I1, I2, IN, IPV, IS, IW1, IW2, LZA, LZM, MOX, N, NAUX, NV,
     $        jummy
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(13),LZM)
C
C---- ZINDEX      as of 1984 Apr 24
      integer     MAUXZI
      common      /ZINDEX/ MAUXZI
C     Auxiliary Z-scale index, for input processing.
C     .
C     !DASH
      external  PISH, ARRAN, MACAW, ZERO1, BULL, ALFALFA, WGIVE, HI, BYE
      intrinsic abs
C
      dimension W(*)
C
C               LZM is the largest ZAUX length (i.e. the largest LZA);
C               NZM is the largest value of i such that LZA(i).ne.0.
C
C               LZA(50), VECTOR(N), Z(N), ZAUX(LZM,NZM)
      dimension LZA(*),  VECTOR(*), Z(*), ZAUX(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1), IPV   ),(IN( 2),IW1   ),(IN( 3),IW2   )
C     !EJECT
C
      call HI ('CREAM')
C     !BEG
C     (Get, and allocate, W allotment)
      call MACAW     (IN, IS, MOX, 'CREAM')
C
C---- Establish ZAUX-table index
      NAUX = MAUXZI
      call PISH      (NAUX, QNAME, I1, I2)
C
      if(NAUX.le.0) then
C----   Read vector directly
        call ARRAN   (1, VECTOR, jummy, N, QNAME)
      else
C----   Read preliminary version of vector
        NV = abs(LZA(NAUX))
        call ZERO1   (W(IPV),NV)
        call ARRAN   (1, W(IPV), jummy, NV, QNAME)
C
C----   Extra(inter)polate to standard Z
        call BULL    (QNAME, I1, I2, ZAUX, NAUX, LZM, W(IPV), NV,
     $                Z, VECTOR, W(IW1), W(IW2))
C----   Print version as read (i.e. preliminary) if required
        call ALFALFA (LZA, ZAUX, NAUX, LZM, NV, W(IPV), QNAME, I1, I2)
      end if
C
C     (Give back W allotment
      call WGIVE     (W,'CREAM')
C     !END
      call BYE ('CREAM')
C
      return
      end
