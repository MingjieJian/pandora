      subroutine ADIGE
     $(LU,CGR,YH,LHHSE,KASE)
C
C     Rudolf Loeser, 1982 Nov 04
C---- Prints, for ZAPPY.
C     (This is version 3 of ADIGE.)
C     !DASH
      save
C     !DASH
      real*8 CGR, YH
      integer KASE, LHHSE, LU
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ADIGE')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100) CGR,YH
  100   format(' ',1PE13.5,3X,'Gravity ratio'/
     $         ' ',  E13.5,3X,'Helium-to-Hydrogen ratio')
        if(KASE.ne.1) then
          write (LU,101) LHHSE
  101     format(' ',I13,3X,'LHHSE, reference depth index of H & M')
        end if
        call LINER (2, LU)
      end if
C     !END
      call BYE ('ADIGE')
C
      return
      end
