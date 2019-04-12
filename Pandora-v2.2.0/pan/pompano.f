      subroutine POMPANO
     $(XLAM,WTAB,FIELD)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Makes a wavelength label.
C     !DASH
      save
C     !DASH
      real*8 APM, F, ONE, P, THSND, WTAB, XLAM
      integer NF
      character BLANK*1, FIELD*29, PRFIX*5
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
      external  SIPRFX, ENCODED, HI, BYE
      intrinsic abs
C
      data APM,THSND /1.D10, 1.D3/
C
      call HI ('POMPANO')
C     !BEG
      FIELD = BLANK
      call ENCODED (WTAB,FIELD(3:14),12,12,1,NF)
C
      call SIPRFX  ((abs(XLAM)/APM),F,P,2,PRFIX)
      if((F.ge.ONE).and.(F.lt.THSND)) then
        write (FIELD(15:23),100) F
  100   format(F9.3)
      else
        FIELD(15:23) = BLANK
      end if
      FIELD(25:29) = PRFIX
C     !END
      call BYE ('POMPANO')
C
      return
      end
