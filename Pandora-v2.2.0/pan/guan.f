      subroutine GUAN
     $(NSL,XNUK,XNU,WNUK,WNU)
C
C     Rudolf Loeser, 1992 Mar 27
C---- Sets up wavenumber-equivalents of level frequencies.
C     (This is version 3 of GUAN.)
C     !DASH
      save
C     !DASH
      real*8 CON19, WNU, WNUK, XNU, XNUK, ZERO
      integer J, NSL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, HI, BYE
C
C               XNU(NSL), WNU(NSL)
      dimension XNU(*),   WNU(*)
C
      call HI ('GUAN')
C     !BEG
      call RIGEL (19, CON19)
C
      if(WNUK.le.ZERO) then
        WNUK = CON19*XNUK
      end if
C
      if(WNU(1).ne.ZERO) then
        WNU(1) = ZERO
      end if
C
      do 100 J = 2,NSL
        if(WNU(J).le.ZERO) then
          WNU(J) = CON19*XNU(J)
        end if
  100 continue
C     !END
      call BYE ('GUAN')
C
      return
      end
