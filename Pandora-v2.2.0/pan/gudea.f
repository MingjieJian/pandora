      subroutine GUDEA
     $(Z,ZNDW,NDW,NO)
C
C     Rudolf Loeser, 1989 Jul 25
C---- Prints NDW and associated stuff.
C     (This is version 2 of GUDEA.)
C     !DASH
      save
C     !DASH
      real*8 Z, ZERO, ZNDW
      integer NDW, NO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINER, HI, BYE
C
C               Z(N)
      dimension Z(*)
C
      call HI ('GUDEA')
C     !BEG
      if(NO.gt.0) then
        call LINER (2,NO)
        if(ZNDW.ne.ZERO) then
          write (NO,100) NDW,Z(NDW),ZNDW
  100     format(' ',' Index of reference Doppler width (NDW)',I20,
     $               12X,'Z(NDW)',1PE12.4,:,14X,'ZNDW',E12.4)
        else
          write (NO,100) NDW,Z(NDW)
        end if
      end if
C     !END
      call BYE ('GUDEA')
C
      return
      end
