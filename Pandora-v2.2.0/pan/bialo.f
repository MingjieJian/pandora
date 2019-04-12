      subroutine BIALO
     $(IMAGE,NO,GRID,NG,LGRD,XL,XR,NV,TIT)
C
C     Rudolf Loeser, 1998 Aug 24
C---- Prints the plot IMAGE, for OLBIA.
C     !DASH
      save
C     !DASH
      real*8 GRID, HALF, VAL, XL, XM, XR
      integer I, J, LGRD, NFL, NFR, NG, NO, NV, jummy
      character BLANK*1, IMAGE*(*), LAB*10, LINE*117, QXL*16, QXR*16,
     $          TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external  KWHERE, KGIVE, ENCODED, HI, BYE
      intrinsic min
C
C               GRID(NG), LGRD(NG)
      dimension GRID(*),  LGRD(*)
C
      call HI ('BIALO')
C     !BEG
      XM = HALF*(XL+XR)
      do 100 I = 1,NG
        VAL = log10(GRID(I))
        call KWHERE (IMAGE, XM, VAL, jummy, LGRD(I))
  100 continue
C
      J = 1
      do 103 I = 1,NV
        call KGIVE  (IMAGE, I, LINE)
        LAB = BLANK
        if(I.eq.LGRD(J)) then
          write (LAB,101) GRID(J)
  101     format(F9.2,1X)
          J = min((J+1),NG)
        end if
        write (NO,102) LAB,LINE
  102   format(' ',A10,A117)
  103 continue
C
      call ENCODED  (XL, QXL, 16, 6, 1, NFL)
      call ENCODED  (XR, QXR, 16, 6, 1, NFR)
C
      LAB  = BLANK
      LINE = BLANK
      LINE(       :NFL) = QXL(17-NFL:)
      LINE(118-NFR:   ) = QXR(17-NFR:)
C
      write (NO,102) LAB,LINE
C     !END
      call BYE ('BIALO')
C
      return
      end
