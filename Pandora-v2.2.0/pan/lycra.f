      subroutine LYCRA
     $(N,NT,YBAR,EP,BS,AW,CHI)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Computes CHI, for TULIP.
C     !DASH
      save
C     !DASH
      real*8 AW, BS, CHI, EP, YBAR
      integer I, IL, IU, J, JUL, KLIN, N, NT
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
      external ZERO1, PET, INTRANS, CARLY, HI, BYE
C
C               YBAR(N,NT), EP(N,NT), BS(N,NT), CHI(N,NT), AW(N,NT)
      dimension YBAR(N,*),  EP(N,*),  BS(N,*),  CHI(N,*),  AW(N,*)
C     !EJECT
C
      call HI ('LYCRA')
C     !BEG
      call ZERO1       (CHI, (N*NT))
C
      do 101 J = 1,NT
        call PET       (J)
        if(KLIN.eq.1) then
C
          call INTRANS (IU, IL, 'LYCRA', JUL)
          do 100 I = 1,N
            call CARLY (EP(I,JUL), AW(I,JUL), YBAR(I,JUL), BS(I,JUL),
     $                  CHI(I,JUL))
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('LYCRA')
C
      return
      end
