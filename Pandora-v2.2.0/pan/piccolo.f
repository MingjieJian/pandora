      subroutine PICCOLO
     $(NO,COP,TAU,GTN,S,TAUS,GTS,SS)
C
C     Rudolf Loeser, 1980 Aug 21
C---- Prints LTE results, for ALLOD.
C     (This is version 2 of PICCOLO.)
C     !DASH
      save
C     !DASH
      real*8 COP, CR, CS, CT, FR, FRS, GTN, GTS, S, SS, TAU, TAUS, ZERO
      integer I, IL, IU, N, NO
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINER, DIVIDE, LRATIO, SHIM, HI, BYE
C
C               TAU(N), GTN(N), S(N), TAUS(N), GTS(N), SS(N), COP(N)
      dimension TAU(*), GTN(*), S(*), TAUS(*), GTS(*), SS(*), COP(*)
C
      call HI ('PICCOLO')
C     !BEG
      if(NO.gt.0) then
        call LINER      (4,NO)
        write (NO,100) IU,IL
  100   format(' ','LTE Comparisons for Transition',I3,'/',I2///
     $         ' ',3X,3(34X,'log')/
     $         ' ',12X,'TAUS',9X,'TAU',6X,'TAUS/TAU',8X,'RS',11X,'R',
     $             9X,'RS/R',10X,'B',12X,'S',10X,'B/S')
        call LINER      (1,NO)
C
        do 102 I = 1,N
          call DIVIDE   (COP (I),GTN(I),FR )
          call DIVIDE   (COP (I),GTS(I),FRS)
          call LRATIO   (FRS    ,FR    ,CR )
          call LRATIO   (SS  (I),S  (I),CS )
          if(I.eq.1) then
            CT = ZERO
          else
            call LRATIO (TAUS(I),TAU(I),CT )
          end if
          write (NO,101) I,TAUS(I),TAU(I),CT,FRS,FR,CR,SS(I),S(I),CS
  101     format(' ',I3,3(3X,1P2E13.5,0PF8.3))
          call SHIM     (I,5,NO)
  102   continue
      end if
C     !END
      call BYE ('PICCOLO')
C
      return
      end
