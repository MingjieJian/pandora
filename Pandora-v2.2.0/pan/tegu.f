      subroutine TEGU
     $(LYMIT,LNLIM,EXLYM,TGLYM,YL,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Prints lyman source function input data, for LINTEL.
C     !DASH
      save
C     !DASH
      real*8 EXLYM, TGLYM, YL
      integer LNLIM, LYMIT, NO
      character TIT*10
C     !DASH
      external HAKO, LINER, HI, BYE
C
      call HI ('TEGU')
C     !BEG
      call HAKO  (YL,TIT)
      call LINER (1,NO)
      write (NO,101) LYMIT,LNLIM,EXLYM,TGLYM,TIT
  101 format(' ','LYMITER ',I3,23X,'number of Lyman iterations'/
     $       ' ','LN      ',I3,23X,'limiting index for Saturation ',
     $                      'Approximation for Lyman Source Function'/
     $       ' ','EXLYM   ',1PE12.4,14X,'parameters for Saturation ',
     $                      'Approximation for Lyman Source Function'/
     $       ' ','TGLYM   ',E12.4/
     $       ' ','YL      ',2X,A10,14X,'Lyman Source Function method ',
     $                      'selection parameter')
C     !END
      call BYE ('TEGU')
C
      return
      end
