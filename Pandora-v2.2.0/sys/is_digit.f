      subroutine IS_DIGIT
     $(CHAR,YES)
C
C     Rudolf Loeser, 2006 Jun 07
C---- Returns YES = .true. if CHAR is a decimal digit, 0-9.
C     !DASH
      save
C     !DASH
      integer I
      logical YES
      character CHAR*1, TAB*1
C     !DASH
      dimension TAB(10)
C
      data TAB /'0','1','2','3','4','5','6','7','8','9'/
C
C     !BEG
      do 100 I = 1,10
        YES = CHAR.eq.TAB(I)
        if(YES) then
          goto 101
        end if
  100 continue
  101 continue
C     !END
C
      return
      end
