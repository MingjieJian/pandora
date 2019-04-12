      subroutine ELDOL
     $(IND,TAU,LINE,KOUNT)
C
C     Rudolf Loeser, 1983 May 19
C---- Encodes depth-of-formation data, for eclipse intensity printouts.
C     !DASH
      save
C     !DASH
      real*8 CRIT, TAU, TONE, ZERO
      integer I, IND, KOUNT
      character BLANK*1, LINE*11
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
      external  HI, BYE
      intrinsic abs
C
C               IND(10), TAU(10), LINE(10)
      dimension IND(*),  TAU(*),  LINE(*)
C
      data CRIT /9.999D0/
C     !EJECT
C
      call HI ('ELDOL')
C     !BEG
      do 102 I = 1,KOUNT
C
        LINE(I) = '    :      '
C
        if((IND(I).gt.-1000).and.(IND(I).lt.+1000)) then
          write (LINE(I)(1:4),100) IND(I)
  100     format(I4)
        else
          LINE(I)(1:4) = BLANK
        end if
C
        TONE = TAU(I)
        if(abs(TONE).gt.CRIT) then
          if(TONE.gt.ZERO) then
            TONE =  CRIT
          else
            TONE = -CRIT
          end if
        end if
        write (LINE(I)(6:11),101) TONE
  101   format(F6.3)
C
  102 continue
C     !END
      call BYE ('ELDOL')
C
      return
      end
