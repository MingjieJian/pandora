      subroutine NERO
     $(IS,IE,WTAB,NO,P1,P2)
C
C     Rudolf Loeser, 2000 Oct 24
C---- Prints a wavelength header, for THUNDER.
C     (This is version 2 of NERO.)
C     !DASH
      save
C     !DASH
      real*8 APM, F, ONE, P, THSND, WTAB
      integer I, IE, IS, KNT, NO
      character BLANK*1, P1*8, P2*8, PRFIX*5
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  SIPRFX, NEBO, HI, BYE
      intrinsic abs
C
C               WTAB(KM or Nmkuse), P1(14), P2(14)
      dimension WTAB(*),            P1(*),  P2(*)
C
      data APM,THSND /1.D10, 1.D3/
C     !EJECT
C
      call HI ('NERO')
C     !BEG
      write (NO,100) (I,I=IS,IE)
  100 format(' ',18X,'Wavelength in some form of meter, as indicated'//
     $       ' ',15X,14I8)
C
      KNT = 0
      do 101 I = IS,IE
        KNT = KNT+1
        call SIPRFX ((abs(WTAB(I))/APM),F,P,2,PRFIX)
        if((F.ge.ONE).and.(F.lt.THSND)) then
          call NEBO (F,P1(KNT))
          P2(KNT) = '   '//PRFIX
        else
          P1(KNT) = BLANK
          P2(KNT) = BLANK
        end if
  101 continue
C
      write (NO,102) (P1(I),I=1,KNT)
  102 format(' ',3X,'Depth',7X,14A8)
      write (NO,103) (P2(I),I=1,KNT)
  103 format(' ',4X,'(km)',7X,14A8)
C     !END
      call BYE ('NERO')
C
      return
      end
