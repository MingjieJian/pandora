      subroutine JATAKA
     $(NO,XLDT,YLDT,ADT,ALBDT,Z,TDT,NDT,N,DDT,MDTR1,MDTR2,WTD,YFLUX,
     $ TLTR)
C
C     Rudolf Loeser, 1981 May 07
C---- Prints TYPE-2 Dust calculation parameters.
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, DDT, TDT, TLTR, WTD, XLDT, YFLUX, YLDT, Z
      integer I, MDTR1, MDTR2, N, NDT, NO
      character BLANK*1, DC*62, TIT*10, ZC*40
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, HAKO, SHIM, HI, BYE
      intrinsic max
C
C               Z(N), TDT(N), XLDT(NDT), YLDT(NDT), ADT(NDT), ALBDT(NDT)
      dimension Z(*), TDT(*), XLDT(*),   YLDT(*),   ADT(*),   ALBDT(*)
C     !EJECT
C
      call HI ('JATAKA')
C     !BEG
      if(NO.gt.0) then
        write (NO,100)
  100   format(' ',42X,'Opacity',5X,'Scattering'/
     $         ' ',5X,'Wavelength',26X,'Function',9X,'Albedo',
     $             15X,'Depth',4X,'Temperature'/
     $         ' ',12X,'LDT',13X,'Y',17X,'ADT',12X,'ALB',19X,'Z',
     $             10X,'TDUST')
        call LINER    (1,NO)
C
        do 104 I = 1,(max(N,NDT))
          ZC = BLANK
          if(I.le.N) then
            write (ZC,101) Z(I),TDT(I)
  101       format(1PE20.4,0PF15.3)
          end if
C
          DC = BLANK
          if(I.le.NDT) then
            call HAKO (YLDT(I),TIT)
            write (DC,102) XLDT(I),TIT,ADT(I),ALBDT(I)
  102       format(1PE13.5,4X,A10,5X,2E15.3)
          end if
C
          write (NO,103) I,DC,ZC
  103     format(' ',I2,A62,A40)
          call SHIM   (I,5,NO)
  104   continue
C
        call LINER    (1,NO)
        write (NO,105) DDT,MDTR1,MDTR2,WTD,TLTR,YFLUX
  105   format(' ','DDT',F12.5,26X,'MDTR1',I3,7X,'MDTR2',I3/
     $         ' ','WTD',F12.5/
     $         ' ','TLTR',F11.5/
     $         ' ','YFLUX',F10.5)
      end if
C     !END
      call BYE ('JATAKA')
C
      return
      end
