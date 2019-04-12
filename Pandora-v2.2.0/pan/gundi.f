      subroutine GUNDI
     $(NO,MHM,XLHM,AHM,YHM,IQHMS,IQHMO)
C
C     Rudolf Loeser, 1985 May 08
C---- Prints H- wavelengths.
C     !DASH
      save
C     !DASH
      real*8 AHM, XLHM, YHM
      integer I, IE, IQHMO, IQHMS, IS, KAR, MHM, NO
      character LINE*125
C     !DASH
      external  PADMA, LINER, HAKO, HI, BYE
      intrinsic min
C
C               XLHM(MHM), AHM(MHM), YHM(MHM)
      dimension XLHM(*),   AHM(*),   YHM(*)
C
      call HI ('GUNDI')
C     !BEG
      if((NO.gt.0).and.(MHM.gt.0).and.(IQHMS.gt.0)) then
        call PADMA    (NO,'H- Calculation')
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+8,MHM)
          call LINER  (1,NO)
          write (NO,101) (XLHM(I),I=IS,IE)
  101     format(' ','LHM  ',1P8E15.7)
          write (NO,102) (AHM(I),I=IS,IE)
  102     format(' ','AHM  ',8F15.5)
C
          LINE = 'YHM  '
          KAR  = -10
          do 103 I = IS,IE
            KAR = KAR+15
            call HAKO (YHM(I),LINE(KAR+6:KAR+15))
  103     continue
          write (NO,104) LINE
  104     format(' ',A125)
        if(IE.lt.MHM) goto 100
C
        if(IQHMO.le.0) then
          call LINER  (1,NO)
          write (NO,105)
  105     format(' ','This basic wavelength table will be augmented ',
     $               'with any other wavelength values falling ',
     $               'within its range.')
        end if
      end if
C     !END
      call BYE ('GUNDI')
C
      return
      end
