      subroutine NAGA
     $(DMPN,DMPX,IND,LL,MODE)
C
C     Rudolf Loeser, 1981 Dec 03
C---- Dump headers and/or dumps, for MAPLE.
C     !DASH
      save
C     !DASH
      integer IL, IND, IU, KAR, LL, LUEO, MO, MODE
      logical DMPN, DMPX
      character BLANK*1, BUF*120
C     !COM
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
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external LINER, ORTS, MESHED, MASHED, HI, BYE
C
      call HI ('NAGA')
C     !BEG
      if(DMPN.and.(IND.gt.0)) then
C----   Write PERDUMPn header
        call LINER    (1, LUEO)
        write (LUEO,100) LL,IND
  100   format(' ',4('*****'),' Contributions from',I3,'. Frequency ',
     $             4('*****'),3X,'IND=',I5)
C
      else if(DMPX.and.(MO.gt.0)) then
        if(MODE.eq.2) then
C----     Save/print line for IPEX dump
          write (BUF(KAR+1:KAR+30),101) IU,IL,IND
  101     format(' ','Transition',I3,',',I2,3X,'IND=',I3)
          KAR = KAR+30
          call ORTS   (KAR, BUF, 'NAGA', 0)
C
        else if(MODE.eq.1) then
C----     Initialize
          KAR = 0
          BUF = BLANK
          call MESHED ('NAGA', 2)
C
        else if(MODE.eq.3) then
C----     Flush last of dump (if needed)
          call ORTS   (KAR, BUF, 'NAGA', 1)
          call MASHED ('NAGA')
        end if
      end if
C     !END
      call BYE ('NAGA')
C
      return
      end
