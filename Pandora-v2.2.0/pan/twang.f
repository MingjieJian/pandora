      subroutine TWANG
     $(F1,FR,IS,IE,LYM,TPOP,KISLV,KONFORM,NO,PLTID)
C
C     Rudolf Loeser, 1974 Dec 30
C---- Prints reserved contributions, for SHARON.
C     !DASH
      save
C     !DASH
      real*8 F1, FR
      integer IE, IS, KISLV, KN, KONFORM, NO
      logical DOIT, F1ZERO, FRZERO, LYM
      character LINE*27, PLTID*1, TPOP*3
C     !DASH
      external PING, NAUGHTD, HI, BYE
C
C               F1(N), FR(N)
      dimension F1(*), FR(*)
C
      dimension PLTID(4)
C
      call HI ('TWANG')
C     !BEG
      if(LYM) then
        KN = (IE-IS)+1
        call NAUGHTD (F1(IS), 1, KN, F1ZERO)
        call NAUGHTD (FR(IS), 1, KN, FRZERO)
C
        DOIT = (.not.F1ZERO).or.(.not.FRZERO)
C
        if(DOIT) then
          write (LINE,100) TPOP,KISLV,PLTID(3)
  100     format(   A3,' Bound-free, Level',I3,' ',A1,' ')
          call PING  (NO, KONFORM, LINE, F1(IS), KN)
C
          write (LINE,101) TPOP,PLTID(4)
  101     format(2X,A3,' Bound-free, others',  ' ',A1,' ')
          call PING  (NO, KONFORM, LINE, FR(IS), KN)
        end if
      end if
C     !END
      call BYE ('TWANG')
C
      return
      end
