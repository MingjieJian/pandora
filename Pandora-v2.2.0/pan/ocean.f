      subroutine OCEAN
     $(XLAM,YNT,BT)
C
C     Rudolf Loeser, 1973 Apr 03
C---- Computes Brightness Temperature, from Intensity /Hz.
C     !DASH
      save
C     !DASH
      real*8 BT, CON5, CON6, CRIT, X, XLAM, Y, YNT, ZERO
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, RIGEL, QLOG1, HI, BYE
C
      data KILROY, CON5, CON6 /.true., 0.D0, 0.D0/
      data CRIT /1.D-100/
C
      call HI ('OCEAN')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL  (5,CON5)
        call RIGEL  (6,CON6)
      end if
C
      if(YNT.le.CRIT) then
        BT = ZERO
      else
        call DIVIDE (CON6,((XLAM**3)*YNT),X)
        call QLOG1  (X,Y)
        call DIVIDE (CON5,(XLAM*Y),BT)
      end if
C     !END
      call BYE ('OCEAN')
C
      return
      end
