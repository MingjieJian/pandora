      subroutine CANVAS
     $(IMAGE,NV,NH)
C
C     Rudolf Loeser, 1968 Jul 26
C---- Initializes the CHECK graph.
C     !DASH
      save
C     !DASH
      real*8 CQ1, CQ2, CQ3, CQ4, DEL, ONE, PLOTM, PLOTP, SIX, YH, YL,
     $       ZERO
      integer NH, NV
      logical GOOD
      character IMAGE*(*), PERIOD*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 7),SIX   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
C     !DASH
      external KINIT, KRIGIA, MONKEY, SPECK, KLINEC, HI, BYE
C
      data CQ1,CQ2,CQ3,CQ4 /1.9D1, 2.D1, 1.002D0, 9.98D-1/
C     !EJECT
C
      call HI ('CANVAS')
C     !BEG
      YL = ONE
      YH = NV
C---- Zero out graph area, and put border around it
      call KINIT    (IMAGE,ZERO,SIX,YH,YL,NV,NH,PERIOD,GOOD)
      if(.not.GOOD) then
        call KRIGIA (ZERO,SIX,YH,YL,NV,NH)
      end if
C---- Add grid lines
      call MONKEY   (IMAGE,ZERO,SIX,ONE,YH,YL,PERIOD,1)
      call MONKEY   (IMAGE,ZERO,(YH+CQ1),CQ2,ZERO,SIX,PERIOD,2)
C---- Add significance limits
      call SPECK    (CQ3,DEL,PLOTP)
      PLOTP =  SIX+PLOTP
      call SPECK    (CQ4,DEL,PLOTM)
      PLOTM = -PLOTM
      call KLINEC   (IMAGE,PLOTP,YL,PLOTP,YH,ALPHS(9),0)
      call KLINEC   (IMAGE,PLOTM,YL,PLOTM,YH,ALPHS(9),0)
C     !END
      call BYE ('CANVAS')
C
      return
      end
