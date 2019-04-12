      subroutine FOLK
     $(KASE,N,TKIN,Z,ABS,TIT)
C
C     Rudolf Loeser, 2006 May 11
C---- Juggles tables and labels to be printed by PONG.
C     !DASH
      save
C     !DASH
      real*8 ABS, TKIN, Z
      integer KASE, N
      character BLANK*1, TIT*6
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external MOVE1, HI, BYE
C
C               TKIN(N), ABS(N), Z(N), TIT(2)
      dimension TKIN(*), ABS(*), Z(*), TIT(*)
C
      call HI ('FOLK')
C     !BEG
      if(KASE.eq.1) then
        call MOVE1 (TKIN, N, ABS)
        TIT(1) = BLANK
        TIT(2) = '  TAUK'
      else
        call MOVE1 (Z,    N, ABS)
        TIT(1) = 'Depth '
        TIT(2) = ' (km) '
      end if
C     !END
      call BYE ('FOLK')
C
      return
      end
