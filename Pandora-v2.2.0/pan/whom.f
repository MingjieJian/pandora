      subroutine WHOM
     $(XCBL,XNU,XJNU)
C
C     Rudolf Loeser, 1980 Aug 22
C---- Retrieves background continuum Jnu for Passive Transitions.
C     (This is version 4 of WHOM.)
C     !DASH
      save
C     !DASH
      real*8 WAVE, XCBL, XJNU, XNU
      integer I, IL, IU, IUL, KKJNU, KLIN, KTAB, N, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
C     !DASH
      external PET, ANGIE, PLUG, INTRANS, MOVE1, ZERO1, HI, BYE
C
C               XCBL(Miklen), XNU(NL), XJNU(N,NT)
      dimension XCBL(*),      XNU(*),  XJNU(N,*)
C
      dimension KTAB(2)
C
      data KTAB /1, 19/
C
      call HI ('WHOM')
C     !BEG
      do 100 I = 1,NT
C
        call PET     (I)
        call INTRANS (IU,IL,'WHOM',IUL)
C
        if(KLIN.eq.2) then
          call ANGIE ((XNU(IU)-XNU(IL)),WAVE)
          call PLUG  (WAVE,KTAB,2,XCBL)
          call MOVE1 (XCBL(KKJNU),N,XJNU(1,IUL))
C
        else
          call ZERO1 (XJNU(1,IUL),N)
        end if
C
  100 continue
C     !END
      call BYE ('WHOM')
C
      return
      end
