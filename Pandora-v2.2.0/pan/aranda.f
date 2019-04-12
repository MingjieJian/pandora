      subroutine ARANDA
     $(NO,N,IU,IL,ICE,WVL,KTKIN,REFLM,EDGTO,BADGTO,EDINT,EDTAU,EDTAUM,
     $ LDL,VZERO,LSTMP,EQUL,SMTH)
C
C     Rudolf Loeser, 1984 Dec 03
C---- Prints a heading, for CONDOR.
C     (This is version 2 of ARANDA.)
C     !DASH
      save
C     !DASH
      real*8 REFLM, WVL
      integer ICE, IL, IU, JCE, K, KTKIN, LDL, LSTMP, N, NO
      logical BADGTO, EDGTO, EDINT, EDTAU, EDTAUM, EQUL, SMTH, VZERO
      character DTIT*4, ESIG*28, LINE*96, LONE*40, Q*55
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ARANDA')
C     !BEG
      call LINER (1, NO)
      write (NO,100) IU,IL
  100 format  (' ','Results of the line-center TAU Calculation, ',
     $             'along the normal direction, for transition (',I2,
     $             '/',I2,')',17X,'(Option TAUPRNT)')
      call LINER (3, NO)
      write (NO,101) WVL,IU,IL
  101 format(' ',3X,'   WVL = "line center" (i.e. at XI = DL = 0) =',
     $              1PE24.16,' (Angstroms)'/
     $       ' ',3X,'         Note: "line center" is computed from ',
     $              'the level frequencies, i.e. from NU(',I2,') - ',
     $              'NU(',I2,').')
      call LINER (1, NO)
      if(KTKIN.eq.1) then
        write (NO,102) REFLM
  102   format(' ',3X,'  TAUK = Continuum optical depth at ',1PE15.8,
     $                          ' Angstroms')
        DTIT = 'TAUK'
      else
        write (NO,103)
  103   format(' ',3X,'     Z = Geometrical depth (km)')
        DTIT = '   Z'
      end if
      write (NO,104) IU,IU,IL,IL
  104 format  (' ',3X,'   VXS = Expansion velocity (km/s)'/
     $         ' ',3X,' N(',I2,') = Level ',I2,' population'/
     $         ' ',3X,' N(',I2,') = Level ',I2,' population'/
     $         ' ',3X,'   PHI = line-center values of wavelength-',
     $                'dependent Line Profile function, '
     $                'Voigt (U,a)')
C     !EJECT
      if(BADGTO) then
        write (NO,105)
  105   format(' ',3X,'   GTN = KL/PHI; * = "bad" values, printed ',
     $             'for reference only - they were edited out',
     $             T101,4('*******'))
      else
        write (NO,106)
  106   format(' ',3X,'   GTN = KL/PHI')
      end if
      if(EDGTO) then
        write (NO,107)
  107   format(' ',12X,'Note that GTN and S can be negative when the ',
     $             'upper level is overpopulated, and that such ',
     $             'negative'/
     $         ' ',12X,'values of S are replaced by positive ',
     $             'interpolated values when option SDIRECT is ON.')
      end if
      if(LSTMP.le.0) then
        if(EQUL.and.(.not.SMTH)) then
          Q = '.)'
        else
          if(.not.EQUL) then
            if(SMTH) then
              Q = '. Note: STIMA and STIMB differ; STIM was smoothed.)'
            else
              Q = '. Note: STIMA amd STIMB differ.)'
            end if
          else
            Q = '. Note: STIM was smoothed.)'
          end if
        end if
        write (NO,108) Q
  108   format(' ',12X,'(To see STIM details, set LSTMP = 1',A)
      end if
C     JCE = ICE  ; changed as follows, 2006 Mar 03
      JCE = 0
      if(JCE.eq.0) then
        write (NO,109) 'opacity'
  109   format  (' ',3X,'    KL = PHI*GTN, calculated line center ',
     $                  'opacity'/
     $           ' ',3X,'   KPC = Total background ',A,' at line ',
     $                  'center')
      else
        write (NO,109) 'pure absorption'
      end if
      if(EDINT) then
        write (NO,110)
  110   format(' ',3X,'   KPT = KL+KPC, total opacity; negative ',
     $             'values printed for reference - they were edited',
     $             T101,4('*******'))
      else
        write (NO,111)
  111   format(' ',3X,'   KPT = KL+KPC, total opacity')
      end if
C     !EJECT
      LINE = '   TAU ='
      K    = 8
      if(.not.VZERO) then
        LINE = LINE(:K)//' Unshifted'
        K    = K+10
      end if
      if(K.eq.8) then
        LINE = LINE(:K)//' Line center optical depth'
      else
        LINE = LINE(:K)//' line center optical depth'
      end if
      K = K+26
      if(LDL.gt.1) then
        LONE = ' for the strongest component line only,'
      else
        LONE = ','
      end if
      if(EDTAU) then
        LINE = LINE(:K)//', edited'
        ESIG = '****************************'
      else
        ESIG = '                            '
      end if
      write (NO,112) LINE,ESIG,LONE
  112 format(' ',3X,A96,T101,A28/
     $       ' ',3X,'         (TAU is not printed here, but in the ',
     $              'following section next to S, the line source ',
     $              'function.)'/
     $       ' ',3X,' TAU-P = Optical depth at the maximum value ',
     $              'of PHI',A/
     $       ' ',3X,'         computed with U=0 (thus ignoring any ',
     $              'expansion velocity) (TAUP = TAU for a single ',
     $              'stationary unblended line)')
      if(EDTAUM) then
        write (NO,113)
  113   format(' ',3X,' TAU-M = Mean optical depth (like TAU, but ',
     $             'with PHI=1); non-increasing regions were edited ',
     $             'before printing',T101,4('*******'))
      else
        write (NO,114)
  114   format(' ',3X,' TAU-M = Mean optical depth (like TAU, but ',
     $             'with PHI=1)')
      end if
      call LINER (2, NO)
      write (NO,115) DTIT,IU,IL
  115 format  (' ',9X,A4,7X,'VXS',5X,'N(',I2,')',5X,'N(',I2,')',
     $             8X,'PHI',8X,'GTN',9X,'KL',8X,'KPC',8X,'KPT',
     $             10X,'TAU-P',9X,'TAU-M')
      call LINER (1, NO)
C     !END
      call BYE ('ARANDA')
C
      return
      end
