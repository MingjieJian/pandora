      subroutine FIVE
     $(IMAGE,LINE,NO,N,NT)
C
C     Rudolf Loeser, 1968 Jul 31
C---- Prints the TAU-scale graph, for ONE.
C     !DASH
      save
C     !DASH
      integer I, IL, IU, KLIN, MINE, N, NO, NT
      character IMAGE*(*), LAB*10, LINE*117
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
C     !DASH
      external  KGIVE, LINER, PET, HI, BYE
C     !EJECT
C
      call HI ('FIVE')
C     !BEG
      LAB  = ' TS'
      MINE = 1
      call KGIVE     (IMAGE,MINE,LINE)
      call LINER     (1,NO)
      write (NO,100) LINE,LAB,LINE,LINE
  100 format(' ',10X,A117/' ',A10,A117/' ',10X,A117)
      call LINER     (1,NO)
C
      do 102 I = 1,NT
        call PET     (I)
C
        if(KLIN.eq.1) then
          write (LAB,101) IU,IL
  101     format(I3,'/',I3,'   ')
          MINE = MINE+1
          call KGIVE (IMAGE,MINE,LINE)
          call LINER (1,NO)
          write (NO,100) LINE,LAB,LINE,LINE
        end if
C
  102 continue
C     !END
      call BYE ('FIVE')
C
      return
      end
