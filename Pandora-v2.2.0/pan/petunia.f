      subroutine PETUNIA
     $(N,NT,ALF,BATA,BDS,SL)
C
C     Rudolf Loeser, 1980 May 02
C---- Drives PEONY, to compute old-style BD ratios.
C     (This is version 2 of PETUNIA.)
C     !DASH
      save
C     !DASH
      real*8 ALF, BATA, BDS, SL
      integer IL, IU, IUL, J, KLIN, N, NT
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
      external INTRANS, PEONY, PET, ZERO1, HI, BYE
C
C               BDS(N,NT), SL(N,NT), ALF(MUL), BATA(N,MUL)
      dimension BDS(N,*),  SL(N,*),  ALF(*),   BATA(N,*)
C     !EJECT
C
      call HI ('PETUNIA')
C     !BEG
      do 100 J = 1,NT
        call PET     (J)
        call INTRANS (IU, IL, 'PETUNIA', IUL)
C
        if(KLIN.eq.1) then
          call PEONY (IU, IL, N, BDS(1,IUL), ALF, BATA, SL(1,IUL))
C
        else
          call ZERO1 (BDS(1,IUL), N)
        end if
  100 continue
C     !END
      call BYE ('PETUNIA')
C
      return
      end
