      subroutine PAULUS
     $(X,IX,W,IW,UPJNU,NONC)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Drives restart data and save data processing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, LUJO, LUMR, LUPR, LURR, NONC
      logical UPJNU
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
      equivalence (LUNITS(30),LUPR )
      equivalence (LUNITS( 4),LURR )
      equivalence (LUNITS(18),LUJO )
C     !DASH
      external HOPE, CARAMEL, CALYPSO, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('PAULUS')
C     !BEG
C---- Normal restart and save data
      call HOPE      (X, IX, W, IW)
C
      if(UPJNU) then
C----   Jnu restart data processing
        call CARAMEL (X, IX, W, IW)
      end if
C
C---- Flush buffers (as needed)
      call CALYPSO   (LUMR)
      call CALYPSO   (LUPR)
      call CALYPSO   (LURR)
      if(NONC.gt.0) then
        call CALYPSO (LUJO)
      end if
C     !END
      call BYE ('PAULUS')
C
      return
      end
