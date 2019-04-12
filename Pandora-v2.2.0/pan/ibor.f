      subroutine IBOR
     $(IS,IE,WTAB,NO,P1,P2)
C
C     Rudolf Loeser, 2000 Oct 23
C---- Prints a wavenumber header, for THUNDER.
C     (This is version 2 of IBOR.)
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer I, IE, IS, KNT, NO
      character P*12, P1*8, P2*8
C     !DASH
      external HI, BYE
C
C               WTAB(KM or Nmkuse), P1(14), P2(14)
      dimension WTAB(*),            P1(*),  P2(*)
C
      call HI ('IBOR')
C     !BEG
      write (NO,100) (I,I=IS,IE)
  100 format(' ',18X,'Wavenumber in /cm (exponent field printed on ',
     $               'second line)'//
     $       ' ',15X,14I8)
C
      KNT = 0
      do 102 I = IS,IE
        KNT = KNT+1
        write (P,101) WTAB(I)
  101   format(1PE12.4)
        P1(KNT) = P(:8)
        P2(KNT) = '    '//P(9:)
  102 continue
C
      write (NO,103) (P1(I),I=1,KNT)
  103 format(' ',3X,'Depth',7X,14A8)
      write (NO,104) (P2(I),I=1,KNT)
  104 format(' ',4X,'(km)',7X,14A8)
C     !END
      call BYE ('IBOR')
C
      return
      end
