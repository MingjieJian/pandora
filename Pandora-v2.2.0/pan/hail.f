      subroutine HAIL
     $(NO,Z,N,MF,ML,WTAB,IWS,ISIG,KODE,KOELS)
C
C     Rudolf Loeser, 1973 Dec 05
C---- Prints, for OOBLECK.
C     !DASH
      save
C     !DASH
      real*8 WTAB, Z
      integer I, IN, ISIG, IWS, J, JE, JS, KLIN, KODE, KOELS, MF, ML, N,
     $        NO
      character BLANK*1, LINE*112, P1*8, P2*8, SIG*2
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  DRIZZLE, THUNDER, WILIWAW, HI, BYE
      intrinsic min
C
C               MM = KM or Nmkuse
C
C               WTAB(MM), IWS(N,MM), ISIG(N,MM), Z(N)
      dimension WTAB(*),  IWS(N,*),  ISIG(N,*),  Z(*)
C
      dimension P1(14), P2(14), SIG(4)
C
      data SIG /'  ', '<<', '- ', '* '/
C     !EJECT
C
      call HI ('HAIL')
C     !BEG
      JE = MF-1
  100 continue
        JS = JE+1
        JE = min((JE+14),ML)
        call THUNDER     (JS,JE,WTAB,KODE,NO,P1,P2)
C
        do 102 I = 1,N
          LINE = BLANK
          KLIN = 0
C
          do 101 J = JS,JE
            call DRIZZLE (IWS(I,J),LINE((KLIN+4):(KLIN+6)))
            IN = ISIG(I,J)+1
            LINE((KLIN+7):(KLIN+8)) = SIG(IN)
            KLIN = KLIN+8
  101     continue
C
          call WILIWAW   (NO,I,N,LINE,Z(I),KOELS,1)
  102   continue
C
      if(JE.lt.ML) goto 100
C     !END
      call BYE ('HAIL')
C
      return
      end
