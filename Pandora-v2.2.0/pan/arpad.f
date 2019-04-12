      subroutine ARPAD
     $(LU,XLCOA,XLCOB,NCB)
C
C     Rudolf Loeser, 1994 Aug 19
C---- Prints CO-lines band limits, for EYAK.
C     !DASH
      save
C     !DASH
      real*8 WNA, WNB, XLCOA, XLCOB
      integer I, LU, NCB
C     !DASH
      external WANDA, LINER, SHIM, HI, BYE
C
C               XLCOA(NCB), XLCOB(NCB)
      dimension XLCOA(*),   XLCOB(*)
C
      call HI ('ARPAD')
C     !BEG
      if((LU.gt.0).and.(NCB.gt.0)) then
        call WANDA     (XLCOA(1),WNA)
        call WANDA     (XLCOB(1),WNB)
        write (LU,100) 1,XLCOA(1),XLCOB(1),WNA,WNB
  100   format(' ',I3,3X,'LCOA =',1PE16.8,3X,'LCOB =',E16.8,
     $            ' Angstroms  (',E16.8,',',E16.8,' wavenumbers)')
C
        if(NCB.gt.1) then
          do 102 I = 2,NCB
            call WANDA (XLCOA(I),WNA)
            call WANDA (XLCOB(I),WNB)
            write (LU,101) I,XLCOA(I),XLCOB(I),WNA,WNB
  101       format(' ',I3,9X,1PE16.8,9X,E16.8,13X,E16.8,1X,E16.8)
            call SHIM  (I,5,LU)
  102     continue
        end if
C
      end if
C     !END
      call BYE ('ARPAD')
C
      return
      end
