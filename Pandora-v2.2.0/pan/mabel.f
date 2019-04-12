      subroutine MABEL
     $(NO,IQAN1)
C
C     Rudolf Loeser, 1998 Dec 30
C---- Prints "Special N1" parameters, for PATCH.
C     (This is version 3 of MABEL.)
C     !DASH
      save
C     !DASH
      real*8 AOWXP, CN1S, SN1CC, WSN1D
      integer I4DEQ, I4DFM, I4DIO, INDX, IQAN1, ITKZA, ITN1R, KBNDS,
     $        KDAMP, KDIAG, MN1, MXPPI, MXTAP, N1MET, N1NUP, N1SPC,
     $        N1UPC, NDSN1, NO
      character BLANK*1, J1MET*1, J4DEQ*1, J4DFM*1, J4DIO*1, JBNDS*1,
     $          JDIAG*1, N1SUP*6, N1UPS*7, SW*9
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(101),MN1  )
      equivalence (KZQ( 91),N1MET)
      equivalence (KZQ(171),N1NUP)
      equivalence (KZQ( 90),KDIAG)
      equivalence (KZQ(170),KBNDS)
      equivalence (KZQ(169),KDAMP)
      equivalence (KZQ(157),ITN1R)
      equivalence (KZQ(161),I4DIO)
      equivalence (KZQ(160),I4DEQ)
      equivalence (KZQ(159),I4DFM)
      equivalence (RZQ(153),SN1CC)
      equivalence (RZQ(155),WSN1D)
      equivalence (RZQ(156),AOWXP)
      equivalence (KZQ(198),NDSN1)
      equivalence (RZQ(176),CN1S )
      equivalence (KZQ(200),MXPPI)
      equivalence (KZQ(201),MXTAP)
      equivalence (KZQ(202),ITKZA)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external  NIBBLE, LINER, HI, BYE
      intrinsic min, max
C
      dimension J1MET(3), JDIAG(3), JBNDS(2), J4DIO(2), J4DFM(2),
     $          J4DEQ(3), SW(2)
C
      data SW /' ( = off)', ' ( = on)'/
C
      call HI ('MABEL')
C     !BEG
      if(IQAN1.le.0) then
        write (NO,100)
  100   format(' ','Option AMDN1 = off   ---   presumably ',
     $             'for good reason.')
      else
        if(N1NUP.eq.1) then
          N1UPS = 'are'
          N1UPC = 3
        else
          N1UPS = 'are not'
          N1UPC = 7
        end if
        if(NDSN1.eq.1) then
          N1SUP = 'is'
          N1SPC = 2
        else
          N1SUP = 'is not'
          N1SPC = 6
        end if
        INDX = min(max(KDAMP,0),1)
        write (NO,101) MN1,N1UPS(:N1UPC),N1NUP,N1SUP(:N1SPC),NDSN1,
     $                 SN1CC,ITN1R,WSN1D,AOWXP,KDAMP,SW(INDX+1)
  101   format(' ','The "Special N1" recalculation extends to depth ',
     $             'index MN1 =',I5//
     $         ' ','The number densities of the ion-of-the-run, and ',
     $             'the associated populations data file,'/
     $         ' ',A,' set equal to the values computed by this ',
     $             '"Special N1" calculation (N1NUP =',I2,')'/
     $         ' ','The "Special N1" calculation ',A,' suppressed in ',
     $             'the first overall iteration (NDSN1 =',I2,').'//
     $         ' ','The N1-recalculation convergence criterion SN1CC',
     $             ' = ',1PE7.1,', the iterations limit ITN1R =',I3/
     $         ' ','The N1- and NK-recalculation weight WSN1D = ',
     $             E12.4/
     $         ' ','The alpha-normalization (He-II) weight exponent ',
     $             'AOWXP = ',E12.4/
     $         ' ','The N1-recalculation damping switch ',
     $             'KDAMP =',I2,A)
C     !EJECT
        call LINER  (1,NO)
        write (NO,102)
  102   format(' ','  Listing of control parameters for solving the ',
     $             '"Special N1" equations (improving NK, N1, and the ',
     $             'upper-level Ns);'/
     $         ' ','  (see also further explanations appearing below ',
     $             ' in the DIFFUSION section of this output file).')
        call NIBBLE  (J1MET, N1MET, 1, 3)
        call NIBBLE  (JDIAG, KDIAG, 3, 5)
        call NIBBLE  (JBNDS, KBNDS, 0, 1)
        call NIBBLE  (J4DIO, I4DIO, 1, 2)
        call NIBBLE  (J4DFM, I4DFM, 1, 2)
        call NIBBLE  (J4DEQ, I4DEQ, 0, 2)
C
        call LINER   (1, NO)
        write (NO,103) J1MET(1),J1MET(2)
  103   format(' ','  N1MET = 1',A,': use exponential method if ',
     $             'possible:  inward if no values of the integrand h ',
     $             'are negative'/
     $         ' ',50X,'outward if no values of the integrand h ',
     $             'are positive'/
     $         ' ',36X,'(if the values of the integrand h are not all ',
     $             'directed inward or outward,'/
     $         ' ',40X,'then use a diagonal method)'//
     $         ' ','  N1MET = 2',A,': use a diagonal method, ',
     $             'as controlled by KDIAG')
        call LINER   (1, NO)
        write (NO,104) JDIAG(1),JDIAG(2)
  104   format(' ',13X,'KDIAG = 3',A,': use 3-diagonal method'//
     $         ' ',13X,'KDIAG = 4',A,': use 4-diagonal method, as ',
     $             'further controlled by I4DIO, I4DEQ, and I4DFM')
        call LINER   (1, NO)
        write (NO,105) J4DIO,J4DEQ,J4DFM
  105   format(' ',26X,'I4DIO = 1',A,': in stationary case, use ',
     $             ' "inward" version'/
     $         ' ',26X,'      = 2',A,': in stationary case, use ',
     $             '"outward" version'/
     $         ' ',26X,'                   (in moving case, use ',
     $             '"inward" or "outward" according to direction of ',
     $             'motion)'//
     $         ' ',26X,'I4DEQ = 0',A,': use    original equations'/
     $         ' ',26X,'      = 1',A,': use alternate-1 equations'/
     $         ' ',26X,'      = 2',A,': use alternate-2 equations'//
     $         ' ',26X,'I4DFM = 1',A,': select the results from the ',
     $             '   Z-formulation'/
     $         ' ',26X,'      = 2',A,': select the results from the ',
     $             'zeta-formulation')
        call LINER   (1, NO)
        write (NO,106) JDIAG(3)
  106   format(' ',13X,'KDIAG = 5',A,': use 5-diagonal method')
        call LINER (1, NO)
        write (NO,107) J1MET(3)
  107   format(' ','  N1MET = 3',A,': use simultaneous method (in ',
     $             'He-I or He-II runs only)')
C     !EJECT
        call LINER (1, NO)
        write (NO,108) JBNDS
  108   format(' ','  KBNDS = 0',A,':  no boundary conditions ',
     $             '(KBNDS fixed = 0 when VELGRAD = off)'/
     $         ' ','  KBNDS = 1',A,': use boundary conditions for ',
     $             'the 3-diagonal, 5-diagonal, or simultaneous ',
     $             'method')
 
        if(N1MET.ne.3) then
          call LINER (1, NO)
          write (NO,109) CN1S,ITKZA,MXPPI,MXTAP
  109     format(' ','  Z-augmentation: CN1S ("N1lim") =',1PE9.2,
     $               ', iterations limit, ITKZA =',I4/
     $           ' ','                  limit of number of added ',
     $               'points per Z-interval, MXPPI =',I3/
     $           ' ','                  limit of total number of ',
     $               'added points, MXTAP =',I5)
        end if
 
 
C
        call LINER (1, NO)
        write (NO,110)
  110   format(' ','  (End of N1 control parameters listing.)')
      end if
C     !END
      call BYE ('MABEL')
C
      return
      end
