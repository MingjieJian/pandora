      subroutine BAAL
     $(NO,INK,XINK,FINK,IRTIS)
C
C     Rudolf Loeser, 1981 May 07
C---- Prints incident radiation tables, for RUPERT.
C     !DASH
      save
C     !DASH
      real*8 BT, FINK, XINK, XLM
      integer I, INK, IRTIS, NO
      character TEXT*41
C     !DASH
      external ANGIE, OCEAN, LINER, SHIM, HI, BYE
C
C               XINK(INK), FINK(INK)
      dimension XINK(*),   FINK(*)
C
      dimension TEXT(3)
C
      data TEXT /'Linear in XINK, linear in FINK.',
     $           'Linear in XINK, linear in log(FINK).',
     $           'Linear in log(XINK), linear in log(FINK).'/
C     !EJECT
C
      call HI ('BAAL')
C     !BEG
      if(NO.gt.0) then
        call LINER   (1,NO)
        write (NO,100)
  100   format(' ','XINK = frequency, in units of 10**15 Hz'/
     $         ' ','LM   = wavelength, in units of Angstroms (printed ',
     $             'for reference)'/
     $         ' ','FINK = Mean Intensity at the atmospheric boundary ',
     $             '(see option INCIFRNT) from an external source, in ',
     $             'units of ergs/cm**2/s/Hz'/
     $         ' ','TB   = corresponding brightness temperature, in ',
     $             'units of K (printed for reference)'//
     $         ' ',24X,'XINK',18X,'LM',16X,'FINK',18X,'TB')
        call LINER   (1,NO)
C
        do 102 I = 1,INK
          call ANGIE (XINK(I),XLM)
          call OCEAN (XLM,FINK(I),BT)
          write (NO,101) I,XINK(I),XLM,FINK(I),BT
  101     format(' ',I8,1P4E20.8)
          call SHIM  (I,5,NO)
  102   continue
C
        call LINER   (1,NO)
        write (NO,103) IRTIS,TEXT(IRTIS)
  103   format(' ','Values of incident radiation at any frequency ',
     $             'are obtained from this table by interpolation.'/
     $         ' ','Three varieties of interpolation are available, ',
     $             'controlled by the input parameter IRTIS.'/
     $         ' ','In this run:'/
     $         ' ','IRTIS =',I1,': ',A/
     $         ' ','The other options are:')
        if(IRTIS.ne.1) then
          write (NO,104) '1',TEXT(1)
        end if
        if(IRTIS.ne.2) then
          write (NO,104) '2',TEXT(2)
        end if
        if(IRTIS.ne.3) then
          write (NO,104) '3',TEXT(3)
        end if
  104   format(' ','IRTIS =',A1,': ',A)
        write (NO,105)
  105   format(' ','In any case, for frequencies beyond the extrema ',
     $             'of the XINK table, the corresponding extrema ',
     $             'of the FINK table will be used.')
      end if
C     !END
      call BYE ('BAAL')
C
      return
      end
