      subroutine NUCLAS
     $(CHAR,MODE,VAL)
C     Rudolf Loeser, 1978 Nov 29
C---- Classifies characters, for NUDEAL.
C     Upon return, if
C     MODE=1, CHAR is a digit, and its value returns in VAL.
C     MODE=2, CHAR is a sign, and VAL=+1.0 or -1.0, as the case may be.
C     MODE=3, CHAR is the decimal point (VAL=0).
C     MODE=4, CHAR is the exponent field flag, and VAL =0.0 if e or E,
C                     or 1.0 if d or D.
C     MODE=5, CHAR is anything else (VAL=0).
C     !DASH
      save
C     !DASH
      real*8 FONE, FZERO, VAL
      integer KAR, MODE, NINE, ZERO
      character BLANK*1, CHAR*1, ESC*1, MINUS*1, PLUS*1, POINT*1, XF*1
C     !COM
      common /NUCONT/ BLANK,POINT,PLUS,MINUS,ESC,XF
      common /NUASCI/ ZERO,NINE
C     !DASH
      intrinsic ichar
C
      dimension XF(4)
C
      data FZERO, FONE /0.D0, 1.D0/
C
C     !BEG
      MODE = 5
      VAL  = FZERO
      KAR  = ichar(CHAR)
      if((KAR.ge.ZERO).and.(KAR.le.NINE)) then
        VAL  =  (KAR-ZERO)
        MODE =  1
      else if(CHAR.eq.MINUS) then
        VAL  = -FONE
        MODE =  2
      else if(CHAR.eq.PLUS) then
        VAL  = +FONE
        MODE =  2
      else if(CHAR.eq.POINT) then
        MODE =  3
      else if((CHAR.eq.XF(1)).or.(CHAR.eq.XF(2))) then
        MODE =  4
      else if((CHAR.eq.XF(3)).or.(CHAR.eq.XF(4))) then
        VAL  = +FONE
        MODE =  4
      end if
C     !END
C
      return
      end
