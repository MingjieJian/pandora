      subroutine ALDEN
     $(LU,IMAGE,NV,ZL,ZH,Z,TE,N)
C
C     Rudolf Loeser, 1992 Dec 30
C---- Prints tables and graph, for BROOM.
C     (This is version 2 of ALDEN.)
C     !DASH
      save
C     !DASH
      real*8 DIV, TE, TVAL, Z, ZH, ZINC, ZL, ZVAL
      integer I, IRET, LU, N, NV
      character IMAGE*(*), LINE*101
C     !DASH
      external KGIVE, LININT, HI, BYE
C
C               TE(N), Z(N)
      dimension TE(*), Z(*)
C
      call HI ('ALDEN')
C     !BEG
      DIV  = NV-1
      ZINC = (ZH-ZL)/DIV
      ZVAL = ZH+ZINC
C
      do 101 I = 1,NV
        ZVAL = ZVAL-ZINC
        call LININT (Z,1,TE,1,N, ZVAL,TVAL, 1,1,IRET)
C
        call KGIVE  (IMAGE,I,LINE)
        write (LU,100) ZVAL,TVAL,LINE
  100   format(' ',1PE13.6,E12.5,1X,A101)
  101 continue
C     !END
      call BYE ('ALDEN')
C
      return
      end
