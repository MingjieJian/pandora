      subroutine PINYON
     $(A,GOOD)
C
C     Rudolf Loeser, 1992 Mar 19
C---- Checks to see whether any A for radiative transitions .le. 0;
C     if yes, sets GOOD = .false.; otherwise, sets GOOD = .true.
C     (This is version 2 of PINYON.)
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer I, IL, IU, IUL, J, KLIN, N, NT
      logical CHECK, GOOD
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external PET, INTRANS, HI, BYE
C
C               A(N,NT)
      dimension A(N,*)
C
      call HI ('PINYON')
C     !BEG
      do 101 J = 1,NT
        call PET       (J)
        CHECK = (KLIN.eq.1)
        if(CHECK) then
          call INTRANS (IU, IL, 'PINYON', IUL)
          do 100 I = 1,N
            GOOD = A(I,IUL).gt.ZERO
            if(.not.GOOD) then
              goto 102
            end if
  100     continue
        end if
  101 continue
C
  102 continue
C     !END
      call BYE ('PINYON')
C
      return
      end
