      subroutine BENJAMN
     $(NW,WTAB,INDXW,IPER,KIPE,NOPAC,LEGEND)
C
C     Rudolf Loeser, 1973 May 17
C---- Prints a contributions summary.
C     !DASH
      save
C     !DASH
      real*8 WTAB
      integer I, IE, INDXW, IPER, IS, J, K, KIPE, LEGEND, LINES, NO,
     $        NOPAC, NW
      character BLANK*1, LINE*105
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  DINOSAW, JOLT, FISH, HI, BYE
      intrinsic min
C
C               IPER(Nopac,Numkon), INDXW(Numkon), WTAB(Numkon)
      dimension IPER(NOPAC,*),      INDXW(*),      WTAB(*)
C     !EJECT
C
      call HI ('BENJAMN')
C     !BEG
C---- Print heading
      call DINOSAW      (NO, LEGEND)
      LEGEND = 1
C
C---- Set up current batch of absorbers
      IE = 0
  100 continue
C
        IS = IE+1
        IE = min(IE+34,NOPAC)
C----   Initialize line counter
        LINES = 40
C
C----   Loop over all wavelengths
        do 102 J = 1,NW
          if(INDXW(J).gt.0) then
C
C----       Set up the current line image of this wavelength
            K = 0
            LINE = BLANK
            do 101 I = IS,IE
              call JOLT (IPER(I,J), LINE((K+1):(K+3)))
              K = K+3
  101       continue
C----       Print the line, and header if needed
            call FISH   (NO, LINES, IS, IE, LINE, J, WTAB, INDXW, KIPE)
C
          end if
  102   continue
C
      if(IE.lt.NOPAC) goto 100
C     !END
      call BYE ('BENJAMN')
C
      return
      end
