      subroutine DOT
     $(POINT,IMAGE,PSYM,LINE,KINT,JXO,IYO)
C
C     Rudolf Loeser, 1968 Jul 26
C---- Enters a point into the CHECK plot.
C     !DASH
      save
C     !DASH
      real*8 DELTA, PLOT, POINT, SIX, THREE, TWO, ZERO
      integer IY, IYO, JX, JXO, KINT, LINE
      character IMAGE*(*), PSYM*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 7),SIX   )
C     !DASH
      external SPECK, KWHERE, KPLOTP, KLINEP, HI, BYE
C
      call HI ('DOT')
C     !BEG
C---- Restrict PLOT=log(POINT-1) to range -3. to 0.
      call SPECK    (POINT,DELTA,PLOT)
C---- Compute coordinate, depending on sign of (POINT-1)
      if(DELTA.ge.ZERO) then
        PLOT =  SIX+PLOT
      else
        PLOT = -PLOT
      end if
C---- If plot falls off central axis, increment Interest Counter
      if(PLOT.ne.THREE) then
        KINT = KINT+1
      end if
C---- Enter plot into graph . . .
      call KWHERE   (IMAGE,PLOT,TWO,JX,IY)
      call KPLOTP   (IMAGE,JX,LINE,PSYM)
      if((JXO+IYO).gt.0) then
C----   . . . and enter connecting line segment
        call KLINEP (IMAGE,JXO,IYO,JX,LINE,PSYM,0)
      end if
C---- Save current line segment end point coordinates
      JXO = JX
      IYO = LINE
C     !END
      call BYE ('DOT')
C
      return
      end
