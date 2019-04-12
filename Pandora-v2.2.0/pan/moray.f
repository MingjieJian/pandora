      subroutine MORAY
     $(NO,LM1,LM3,IZERO,IQRKE,ZH)
C
C     Rudolf Loeser, 1982 Jul 30
C---- Prints a levels data legend, for ATOM.
C     !DASH
      save
C     !DASH
      real*8 RFAC, RFXNC
      integer ICHSW, IQRKE, IZERO, JDCIA, JDCIC, JDCIJ, JDCIS, JDCIV,
     $        JDCIW, JDCP1, JDCPN, JDNUK, JDXNU, MCIOF, NO, NSL, jummy
      logical DEFCI, HYDR, LM1, LM3, ZH
      character QELSM*8, RKE*3, qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 42),RFAC )
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(122),ICHSW)
      equivalence (KZQ(224),MCIOF)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (REST( 5),RFXNC)
C
C---- SUBLET      as of 2006 Dec 04
      character   CITES*64, CQ*20
      dimension   CITES(11), CQ(11)
      common      /SUBLET/ CITES, CQ
C     CI/CE methods source citations for HAMRE et al.
C     .
C     !EJECT
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST( 6),JDCIV)
      equivalence (MEST( 7),JDCIJ)
      equivalence (MEST(20),JDCIC)
      equivalence (MEST(17),JDNUK)
      equivalence (MEST(16),JDXNU)
      equivalence (MEST(22),JDCIA)
      equivalence (MEST(23),JDCP1)
      equivalence (MEST(25),JDCIS)
      equivalence (MEST(26),JDCIW)
      equivalence (MEST(27),JDCPN)
C     !DASH
      external ALGER, ELGAR, VARAN, KAFUE, LINER, PINA, ONOFF, OLDMOR,
     $         MOROLD, HI, BYE
C
      call HI ('MORAY')
C     !BEG
      HYDR = QELSM(1:3).eq.'H  '
C
      call LINER   (1, NO)
      write (NO,100)
  100 format(' ','N O T E S')
C
      call LINER   (1, NO)
      call VARAN   (NO, IZERO, 'JZATOM')
C
      if(LM1.or.LM3) then
        write (NO,101)
  101   format(' ',8X,'* Negative values of the principal quantum ',
     $             'number l signify the following:')
        if(LM1) then
          write (NO,102)
  102     format(' ',10X,'- l = -1 means this level corresponds to ',
     $               'a sum over all l values.')
        end if
        if(LM3) then
          write (NO,103)
  103     format(' ',10X,'- l = -3 means this level corresponds to ',
     $               'a sum over all values of l=4 or higher (i.e. ',
     $               'over hydrogenic levels).')
        end if
        call LINER (1, NO)
      end if
C     !EJECT
      write (NO,104)
  104 format(' ',8X,'* The photoionization cross-section CP is the ',
     $           'threshhold value at the head of the continuum;'/
     $       ' ',10X,'for integrations over frequency, a nu**-3 ',
     $           'variation is assumed if explicit RRCP values '
     $           '(below) are not given.')
C
      if(.not.HYDR) then
        write (NO,105)
  105   format(' ',10X,'If the input value CP(1) = 0, then a ',
     $             'default value is computed using ',
     $             'Verner et al. (1996), ApJ 465, 487.')
        call PINA    (NO, qummy, jummy)
        if(JDCP1.gt.0) then
          write (NO, 106)
  106     format(' ',10X,'This run uses such a default value.')
        end if
        if(JDCPN.gt.0) then
          write (NO, 107)
  107     format(' ',10X,'Other default value(s) of CP were ',
     $               'computed with the hydrogenic approximation.')
        end if
      end if
C
      call ONOFF     (IQRKE, jummy, RKE)
      call LINER     (1, NO)
      write (NO,108) RKE
  108 format(' ',8X,'* RKMULT is used only when option RKINCR ',
     $           '= on; in this run RKINCR = ',A3,'.')
C     !EJECT
      if(HYDR) then
        if((JDNUK.eq.0).or.(JDXNU.lt.(NSL-1))) then
          call LINER (1, NO)
          write (NO,109)
  109     format(' ',8X,'* It is best to use only the default, ',
     $                  'computed values of NU and NUK for ',
     $                  'Hydrogen.')
        end if
      end if
C
      DEFCI = (JDCIJ+JDCIV+JDCIC+JDCIA+JDCIS+JDCIW).gt.0
C
      if(DEFCI) then
        call OLDMOR  (NO, 'CI')
        if(JDCIJ.gt.0) then
          write (NO,110) CITES(5),CQ(5)
  110     format(' ',10X,'- ',A64,5X,A20)
        end if
        if(JDCIV.gt.0) then
          write (NO,110) CITES(4),CQ(4)
        end if
        if(JDCIC.gt.0) then
          write (NO,110) CITES(6),CQ(6)
        end if
        if(JDCIA.gt.0) then
          write (NO,110) CITES(2),CQ(2)
        end if
        if(JDCIS.gt.0) then
          write (NO,110) CITES(1),CQ(1)
        end if
        if(JDCIW.gt.0) then
          write (NO,110) CITES(3),CQ(3)
        end if
      end if
C
      call MOROLD    (NO, MCIOF, 'CI')
C     !EJECT
      if(.not.DEFCI) then
        call LINER (1, NO)
        write (NO,111)
  111   format(' ',8X,'* This run does not use any computed default ',
     $             'values of CI.')
      end if
C
      call LINER   (1, NO)
      write (NO,112)
  112 format(' ',8X,'* Collisional ionization rates are computed ',
     $           'from CI(T)*NE*exp(-h*NU/(k*T)), where NU is the ',
     $           'threshhold value'/
     $       ' ',10X,'at the head of the continuum.')
C
      call LINER   (1, NO)
      if(ZH) then
        write (NO,113)
  113   format(' ',8X,'* Collisions with hydrogen are controlled ',
     $             'with input parameters LCH and ICHSW.')
      else
        write (NO,114) ICHSW
  114   format(' ',8X,'* Added to these rates for collisions with ',
     $             'electrons are rates for collisions with hydrogen ',
     $             'atoms,'/
     $         ' ',10X,'provided that ICHSW = 1 (here ICHSW =',I2,
     $             '). For levels 2 and higher for which LCH = 1 the ',
     $             'added rates are'/
     $         ' ',10X,'from B. Kaulakys, 1985, J.Phys.B, 18, L167. ',
     $             'LCH(1), the LCH value for level 1, is an index ',
     $             'explained in the'/
     $         ' ',10X,'next section of notes for transitional ',
     $             'collisions (below).')
      end if
      write (NO,115)
  115 format(' ',10X,'See also "About PANDORA,", Section 5, Note 136.')
C
      call LINER   (1, NO)
      call ELGAR   (NO, 'CI', DEFCI)
      call LINER   (1, NO)
      call KAFUE   (NO, 'CI')
C
      if(DEFCI.or.(JDCP1.gt.0).or.(JDCPN.gt.0)) then
        call LINER (1, NO)
        call ALGER (NO)
      end if
C     !END
      call BYE ('MORAY')
C
      return
      end
