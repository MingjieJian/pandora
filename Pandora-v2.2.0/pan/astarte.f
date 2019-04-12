      subroutine ASTARTE
     $(MODE,K,NW,WAVES,KTYPE,IADRS)
C
C     Rudolf Loeser, 1984 Feb 07
C---- Selects that subset of the continuum blocks index which pertains
C     to the current transition [IU,IL].
C
C     MODE=1 means: return line core (constant background) wavelength;
C     MODE=2 means: return complete set of PRD (FDB) wavelengths.
C
C     (This is version 3 of ASTARTE.)
C     !DASH
      save
C     !DASH
      real*8 WAVES
      integer IADRS, ICE, IFDB, IL, IU, K, KODE, KTYPE, LINT, LUEO,
     $        MODE, NW
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(15),IFDB )
      equivalence (LINKDS(13),LINT )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external JACOB, LINER, ADONIS, MESHED, ABORT, HI, BYE
C
C               WAVES(K), IADRS(K), KTYPE(K)
      dimension WAVES(*), IADRS(*), KTYPE(*)
C
      data KODE /3/
C
      call HI ('ASTARTE')
C     !BEG
      call JACOB    (KODE, MODE, NW, WAVES, IADRS, KTYPE)
C
      if((MODE.eq.2).and.(NW.ne.K)) then
        call MESHED ('ASTARTE', 1)
        call ADONIS
        call LINER  (2, LUEO)
        write (LUEO,100) IU,IL,K,NW,ICE,LINT,IFDB
  100   format(' ','Trouble with Line (',I2,'/',I2,'): K =',I6,
     $             ' does not equal NW =',I6//
     $         ' ','ICE =',I2,5X,'LINT =',I2,5X,'IFDB =',I2)
        call ABORT
      end if
C     !END
      call BYE ('ASTARTE')
C
      return
      end
