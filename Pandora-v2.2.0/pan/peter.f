      subroutine PETER
     $(N,NL,NT,SET,KIJ,MIJ,PCE,Z,FCE,W,IW)
C
C     Rudolf Loeser, 2004 Mar 26
C---- Examines number densities, and perhaps adjusts FCE.
C     !DASH
      save
C     !DASH
      real*8 FCE, ONE, PCE, SET, W, Z
      integer I, IL, INDX, IU, IUL, IW, KFCEU, KIJ, KOUNT, KRET, LU,
     $        LUEO, MIJ, MO, N, NL, NT
      logical ALERT, DUMP, OK, SMTH
      character LABEL*100, TYPE*3
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
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(69),KFCEU)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  SLING, MESHED, FENSTER, MASHED, KONSTD, INDXNT, SMOOTH,
     $          LINER, PET, HI, BYE
      intrinsic min
C
      dimension W(*), IW(*)
C
C               SET(N,MUL), FCE(N,NT), KIJ(NL,NL), MIJ(NL,NL), PCE(NT),
      dimension SET(*),     FCE(N,*),  KIJ(*),     MIJ(*),     PCE(*),
C
C               Z(N)
     $          Z(*)
C
      data TYPE,INDX,SMTH /'lin', 0, .false./
C     !EJECT
C
      call HI ('PETER')
C     !BEG
C---- Perhaps adjust FCE, and count number of adjustments
      KOUNT = 0
      call SLING      (N, NL, SET, KIJ, MIJ, FCE, PCE, KOUNT)
      KFCEU = min(KOUNT,1)
C
C---- Print ?
      ALERT = .false.
      if(MO.gt.0) then
        call KONSTD   (FCE, 1, (N*NT), ONE, OK)
        ALERT = .not.OK
      end if
      if((KFCEU.gt.0).or.ALERT) then
        if(MO.gt.0) then
          DUMP = .false.
          LU   = MO
          call LINER  (3, LU)
        else
          DUMP = .true.
          LU   = LUEO
          call MESHED ('PETER', 3)
        end if
C----   Print current set
        write (LU,100)
  100   format(' ','Current values of FCE (controlled by option ',
     $             'CEFACTS).')
        call FENSTER  (N, NL, KIJ, FCE, LU)
        if(DUMP) then
          call MASHED ('PETER')
        end if
      end if
C
      if((KFCEU.gt.0).and.SMTH) then
C----   Smooth the adjusted set
        do 102 I = 1,NT
          call PET    (I)
          write (LABEL,101) IU,IL
  101     format('FCE(',I2,'/',I2,'): CE-enhancement factors')
          call INDXNT (IU, IL, OK, IUL)
          call SMOOTH (Z, FCE(1,IUL), N, TYPE, LABEL, INDX, W, IW,
     $                 KRET, OK)
  102   continue
      end if
C     !END
      call BYE ('PETER')
C
      return
      end
