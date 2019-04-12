      subroutine YOLK
     $(N,DGM,VT,PMG,VEC,TAT)
C
C     Rudolf Loeser, 2006 May 11
C---- Juggles tables and labels to be printed by PONG.
C     !DASH
      save
C     !DASH
      real*8 DGM, ONE, PMG, VEC, VT
      integer N
      logical KDGM, ZPMG
      character BLANK*1, TAT*11
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external NAUGHTD, KONSTD, MOVE1, HI, BYE
C
C               DGM(N), VEC(N), VT(N), TAT(4), PMG(N)
      dimension DGM(*), VEC(*), VT(*), TAT(*), PMG(*)
C
      call HI ('YOLK')
C     !BEG
      call NAUGHTD (PMG, 1, N, ZPMG)
      call KONSTD  (DGM, 1, N, ONE, KDGM)
C
      if(.not.ZPMG) then
        TAT(1) = BLANK
        TAT(2) = ' Magnetic  '
        TAT(3) = ' pressure  '
        TAT(4) = '(dyn/cm**2)'
        call MOVE1 (PMG, N, VEC)
      else if(.not.KDGM) then
        TAT(1) = ' Buoyancy  '
        TAT(2) = '  factor   '
        TAT(3) = '   (DGM)   '
        TAT(4) = BLANK
        call MOVE1 (DGM, N, VEC)
      else
        TAT(1) = 'Turbulent  '
        TAT(2) = ' pressure  '
        TAT(3) = ' velocity  '
        TAT(4) = '    (km/s) '
        call MOVE1 (VT,  N, VEC)
      end if
C     !END
      call BYE ('YOLK')
C
      return
      end
