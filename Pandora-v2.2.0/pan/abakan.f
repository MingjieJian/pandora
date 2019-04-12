      subroutine ABAKAN
     $(NO,NWV,WAVES,YWAVE,WAVMN,WAVMX,NWS,SWAVE,NECLP,NSW,SCOW,JM,
     $ XLMM,XMLC,NDV,DWAVE,KBX,BXI,NDWM)
C
C     Rudolf Loeser, 1981 Sep 22
C---- Prints miscellaneous continuum calculations data and controls.
C     !DASH
      save
C     !DASH
      real*8 BXI, DWAVE, SCOW, SWAVE, WAVES, WAVMN, WAVMX, XLMM, XMLC,
     $       YWAVE
      integer JM, KBX, NDV, NDWM, NECLP, NO, NSW, NWS, NWV
C     !COM
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external PADMA, LINER, KAPPA, OSWALD, KERIN, ALBERT, HASH, RINKE,
     $         HI, BYE
C
C               WAVES(NWV), YWAVE(NWV), SWAVE(NWS), XLMM(JM), XMLC(JM),
      dimension WAVES(*),   YWAVE(*),   SWAVE(*),   XLMM(*),  XMLC(*),
C
C               SCOW(NSW), DWAVE(NDV), BXI(KBX)
     $          SCOW(*),   DWAVE(*),   BXI(*)
C
      call HI ('ABAKAN')
C     !BEG
      if(NO.gt.0) then
        call PADMA (NO, 'Other Continuum Wavelengths data')
C
        call LINER (2,NO)
        write (NO,100) WAVEDEL
  100   format(' ','[Required relative difference to render continuum ',
     $             'wavelengths distinct, WAVEDEL =',1PE10.2,'.]')
        if((NDV.gt.0).or.(NWS.gt.0).or.(NSW)) then
          write (NO,101)
  101     format(' ','[However, for SCOW, DWAVE, and DELWAVE, the ',
     $               'required difference is 1.D-08.]')
        end if
C     !EJECT
        if(NWV.gt.0) then
          call KAPPA  (NO, NWV, WAVES, YWAVE, WAVMN, WAVMX, NECLP)
        end if
C
        if(NWS.gt.0) then
          call OSWALD (NO, NWS, SWAVE)
        end if
C
        if(NSW.gt.0) then
          call KERIN  (NO, NSW, SCOW)
        end if
C
        if(NDV.gt.0) then
          call RINKE  (NO, NDV, DWAVE)
        end if
C
        if(JM.gt.0) then
          call ALBERT (NO, JM, XLMM, XMLC)
        end if
C
        call HASH     (NO, KBX, BXI, NDWM)
C
      end if
C     !END
      call BYE ('ABAKAN')
C
      return
      end
