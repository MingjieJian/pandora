      subroutine RUPERT
     $(NO,INK,XINK,FINK,IQINC,IQRSQ,TX,DLU,IQGDS,NCR,XLCR,XICR,XYCR,
     $ OPF,IRTIS)
C
C     Rudolf Loeser, 1973 Apr 12
C---- Prints Incident Radiation data.
C     !DASH
      save
C     !DASH
      real*8 DLU, FINK, OPF, TX, XICR, XINK, XLCR, XYCR
      integer INK, IQGDS, IQINC, IQRSQ, IRTIS, NCR, NO
C     !DASH
      external PADMA, LINER, BAAL, BAIGA, HI, BYE
C
C               XINK(INK), FINK(INK), XLCR(NCR), XICR(NCR), XYCR(NCR)
      dimension XINK(*),   FINK(*),   XLCR(*),   XICR(*),   XYCR(*)
C
      call HI ('RUPERT')
C     !BEG
      if(NO.gt.0) then
        if(IQINC.gt.0) then
          call PADMA   (NO,'Incident Radiation')
C
          if(INK.ge.2) then
            call BAAL  (NO,INK,XINK,FINK,IRTIS)
          else
            call LINER (1,NO)
            write (NO,100) TX
  100       format(' ',1PE29.8,'  TX  = Temperature of Illuminating ',
     $                 'Source')
          end if
C
          call LINER   (1,NO)
          write (NO,101) OPF
  101     format  (' ',1PE29.8,'  OPF = Incident Radiation Extinction ',
     $                 'Factor (extinction = exp [ -OPF * TAU(nu) ])')
C
          call LINER   (1,NO)
          write (NO,102) DLU
  102     format  (' ',1PE29.8,'  DLU = Dilution Factor (multiplies ',
     $                 'FINK)'/
     $             ' ',29X,'        ( 1/r**2 variation is used when ',
     $                 'options RSQUARE=on and INCIFRNT=off)')
        end if
C
        if(NCR.gt.0) then
          call PADMA   (NO,'Incident Coronal Radiation')
          call BAIGA   (NO,XLCR,XICR,XYCR,NCR)
        end if
      end if
C     !END
      call BYE ('RUPERT')
C
      return
      end
