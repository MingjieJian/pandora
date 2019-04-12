      subroutine WANAKA
     $(DDL,LDL,DL,KM,INDL)
C
C     Rudolf Loeser, 2004 Jun 07
C---- Sets up a table of line core indices.
C     (This is version 2 of WANAKA.)
C     !DASH
      save
C     !DASH
      real*8 DDL, DELTA, DL
      integer I, IL, INDL, IU, K, KM, LDL, LOOK, LUEO, NOTE
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LOOKSD, MESHED, VECOUT, ABORT, HI, BYE
C
C               DDL(LDL), DL(KM), INDL(LDL)
      dimension DDL(*),   DL(*),  INDL(*)
C
      data DELTA /1.D-6/
C
      call HI ('WANAKA')
C     !BEG
      do 101 I = 1,LDL
        call LOOKSD   (DL, KM, DELTA, DDL(I), K, NOTE, LOOK)
        if(LOOK.eq.2) then
          K = KM
        end if
C
        if((LOOK.eq.1).or.(LOOK.eq.2)) then
          INDL(I) = K
C
        else
          call MESHED ('WANAKA', 1)
          write (LUEO,100) IU,IL,I,LDL,KM,K,NOTE,LOOK
  100     format(' ','Trouble in WANAKA: trying to find DDL in ',
     $               'DL(',I2,'/',I2,').'//
     $           ' ','I =',I12,', LDL =',I12,', KM =',I12,', K =',I12,
     $               ', NOTE =',I12,', LOOK =',I12)
          call VECOUT (LUEO, DDL, LDL, 'DDL')
          call VECOUT (LUEO, DL,  KM,  'DL' )
          call ABORT
        end if
C
  101 continue
C     !END
      call BYE ('WANAKA')
C
      return
      end
