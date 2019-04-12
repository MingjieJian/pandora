      subroutine TYPHOON
     $(NO,N,MF,ML,IWS,KOELS)
C
C     Rudolf Loeser, 1985 Feb 07
C---- Prints, for OOBLECK.
C     !DASH
      save
C     !DASH
      real*8 ZERO
      integer I, IWS, J, JE, JS, KLIN, KOELS, MF, ML, N, NO
      character BLANK*1, LINE*120
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external  ORKAN, DRIZZLE, WILIWAW, HI, BYE
      intrinsic min
C
C               IWS(N,NW)
      dimension IWS(N,*)
C
      call HI ('TYPHOON')
C     !BEG
      JE = MF-1
  100 continue
C
        JS = JE+1
        JE = min((JE+40),ML)
        call ORKAN       (NO, JS, JE)
C
        do 102 I = 1,N
          LINE = BLANK
          KLIN = 0
C
          do 101 J = JS,JE
            call DRIZZLE (IWS(I,J), LINE((KLIN+1):(KLIN+3)))
            KLIN = KLIN+3
  101     continue
C
          call WILIWAW   (NO, I, N, LINE, ZERO, KOELS,2)
  102   continue
C
      if(JE.lt.ML) goto 100
C     !END
      call BYE ('TYPHOON')
C
      return
      end
