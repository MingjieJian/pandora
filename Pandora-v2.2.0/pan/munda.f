      subroutine MUNDA
     $(Y,NW,BOT,TOP)
C
C     Rudolf Loeser, 1982 Apr 28
C---- Gets vertical plot limits, for LUCCA.
C     (This is version 2 of MUNDA.)
C     !DASH
      save
C     !DASH
      real*8 BOT, C, DEF, ONE, TOP, Y, YMAX, YMIN, ZERO
      integer I, IMAX, IMIN, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MINMAXD, TITAN, HI, BYE
C
C               Y(NW)
      dimension Y(*)
C
      data C /1.D10/
C
      call HI ('MUNDA')
C     !BEG
      call MINMAXD (Y,1,NW,IMIN,IMAX)
      YMIN = Y(IMIN)
      YMAX = Y(IMAX)
C
      if(YMAX.le.ZERO) then
        YMAX = ONE
      end if
      DEF = YMAX/C
      if(YMIN.le.ZERO) then
        YMIN = DEF
      else
        if((YMAX/YMIN).gt.C) then
          YMIN = DEF
        end if
      end if
C
      do 100 I = 1,NW
        if(Y(I).lt.YMIN) then
          Y(I) = ZERO
        end if
  100 continue
      call TITAN   (YMIN,YMAX,BOT,TOP)
C     !END
      call BYE ('MUNDA')
C
      return
      end
