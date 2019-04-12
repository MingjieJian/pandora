      subroutine YORRA
     $(I,KODE,ONAME)
C
C     Rudolf Loeser, 1981 Dec 04
C---- Writes an error message for final processing of ORION Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 ONAME
      integer I, IERR, IL, IU, KODE, LUEO
      character LABEL*40
C     !COM
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  PAPUA, LINER, HI, BYE
      intrinsic mod
C
      call HI ('YORRA')
C     !BEG
      call PAPUA (ONAME, LABEL)
      call LINER (1, LUEO)
      write (LUEO,100) IU,IL,LABEL,I,KODE
  100 format(' ','Transition (',I2,',',I2,'), term omitted from ',
     $           'frequency/angle sum: ',A40,6X,
     $           '(IND=',I6,', KODE=',I2,').')
C
      IERR = KODE
      if(mod(IERR,2).eq.1) then
        write (LUEO,101)
  101   format(' ',20X,'TAU-normal is bad.')
      end if
C
      IERR = IERR/2
      if((IERR.ne.0). and.(mod(IERR,2).eq.1)) then
        write (LUEO,102)
  102   format(' ',20X,'TAU-ray is bad.')
      end if
C     !END
      call BYE ('YORRA')
C
      return
      end
