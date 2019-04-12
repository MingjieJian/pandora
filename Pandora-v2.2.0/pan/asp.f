      subroutine ASP
     $(NO,L,IU,IL,PW,MPROM,F,DPM,LDL,LSAME,COMPUTE)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Prints a heading, for AMULET.
C     !DASH
      save
C     !DASH
      real*8 DPM, F, PW, ZERO
      integer I, IL, IU, J, L, LDL, LSAME, MPROM, NCINM, NO
      logical COMPUTE
      character BLANK*1, QIONM*8, QOM*8, XPSTK*22
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (QEST( 1),QIONM)
      equivalence (LEST(79),NCINM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external LINER, DASHER, HI, BYE
C
C               F(5)
      dimension F(*)
C
      call HI ('ASP')
C     !BEG
      call LINER  (4, NO)
      call DASHER (NO)
      call LINER  (1, NO)
C
C---- Prepare Stark term exponent for printing
      XPSTK = BLANK
      if(F(3).ne.ZERO) then
        write (XPSTK,100) PW
  100   format('(Exponent =',1PE10.3,')')
      end if
C
C---- Prepare Ion Name for printing, centering it in "QOM"
      QOM = BLANK
      J   = NCINM
      if(J.ne.0) then
        if(J.ge.7) then
          QOM = QIONM
        else
          I = (8-J)/2
          QOM(I+1:I+J) = QIONM(1:J)
        end if
      end if
C     !EJECT
C---- Now print heading
      write (NO,101) L,LDL,IU,IL,DPM,F(1),F(2),F(3),MPROM,XPSTK,
     $               F(4),F(5)
  101 format(' ','Damping Parameter for component ',I2,' of ',I2,
     $           ', Transition',I3,'/',I2,37X,'Damping multiplier =',
     $           1PE12.5//
     $       ' ','Broadening half-halfwidths: Radiative',
     $           T50,'- CRD',E15.5/
     $       ' ',13X,'(Angstroms)',4X,'van der Waals',
     $           T50,'- CVW',E15.5/
     $       ' ',T30,'Stark',T50,'- CSK',E15.5,I2,1X,A22/
     $       ' ',T30,'Resonance',T50,'- CRS',E15.5/
     $       ' ',T30,'Ion (Hydrogen only)',T50,'- CIB',E15.5)
      call LINER   (1, NO)
      if(.not.COMPUTE) then
C
        write (NO,102) L,LSAME
  102   format(' ','This ',I2,'. component of DP is the same as ',
     $             'the ',I2,'. component, already printed.')
      else
C
        write (NO,103) QOM
  103   format(' ',34X,'N U M B E R   D E N S I T I E S',
     $             25X,'Contribution to DP',11X,'Voigt'/
     $         ' ',74X,'Damping',15X,'due to',15X,'parameter'/
     $         ' ',46X,A8,5X,'Hydrogen',6X,'Parameter'/
     $         ' ',2X,'I',5X,'Z (km)',6X,'TE (K)',6X,'Electrons',
     $             5X,'Level  1',5X,'Level  1',5X,'(Angstroms)',
     $             3X,'CRD',3X,'CVW',3X,'CSK',3X,'CRS',3X,'CIB',
     $             5X,'A=DP/DW')
        call LINER (1, NO)
C
      end if
C     !END
      call BYE ('ASP')
C
      return
      end
