      subroutine HOOK
     $(NT,JU,JL,JN,MTR,LIM)
C
C     Rudolf Loeser, 1980 May 02
C---- Sets up radiative transition index pairs,
C     in transition order, for PICTURE.
C     (This is version 2 of HOOK.)
C     !DASH
      save
C     !DASH
      integer I, IL, IU, IUL, JL, JN, JU, KLIN, LIM, MTR, NT
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C     !DASH
      external INTRANS, PET,HI,BYE
C
C               JU(LIM), JL(LIM), JN(LIM)
      dimension JU(*),   JL(*),   JN(*)
C
      call HI ('HOOK')
C     !BEG
      MTR = 0
      do 100 I = 1,NT
        call PET       (I)
        if(KLIN.eq.1) then
          call INTRANS (IU,IL,'HOOK',IUL)
          MTR     = MTR+1
          JU(MTR) = IU
          JL(MTR) = IL
          JN(MTR) = IUL
          if(MTR.eq.LIM) goto 101
        end if
  100 continue
  101 continue
C     !END
      call BYE ('HOOK')
C
      return
      end
