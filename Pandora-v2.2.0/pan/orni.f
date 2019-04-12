      subroutine ORNI
     $(IS,IE,WTAB,NO,P1)
C
C     Rudolf Loeser, 2000 Oct 23
C---- Prints a Delta-Lambda header, for THUNDER.
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer I, IE, IS, KNT, NO
      character P1*8
C     !DASH
      external NEBO, HI, BYE
C
C               WTAB(KM or Nmkuse), P1(14)
      dimension WTAB(*),            P1(*)
C
      call HI ('ORNI')
C     !BEG
      write (NO,100) (I,I=IS,IE)
  100 format(' ',18X,'Delta-Lambda in Angstroms, from line center'//
     $       ' ',15X,14I8)
C
      KNT = 0
      do 101 I = IS,IE
        KNT = KNT+1
        call NEBO (WTAB(I),P1(KNT))
  101 continue
C
      write (NO,102) (P1(I),I=1,KNT)
  102 format(' ',3X,'Depth',7X,14A8)
      write (NO,103)
  103 format(' ',4X,'(km)')
C     !END
      call BYE ('ORNI')
C
      return
      end
