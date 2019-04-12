      subroutine OSCAR
     $(NO,IC,IE,CORE,GMMA,XC,XP,XR)
C
C     Rudolf Loeser, 1981 Dec 17
C---- Prints heading for PRD printouts.
C     IC=1 for background preliminaries,  =2 for line source function.
C     (This is version 5 of OSCAR.)
C     !DASH
      save
C     !DASH
      real*8 CORE, GMMA, XC, XP, XR, dummy
      integer IC, ICE, IE, IL, IPRDD, IPRDF, IU, NC, NO
      character LINE*12
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 99),IPRDF)
      equivalence (KZQ( 98),IPRDD)
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
      equivalence (LINKDS( 4),ICE  )
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ABJECT, LINER, HALT, VELCRO, STARER, LUSTER, HI, BYE
C
      call HI ('OSCAR')
C     !BEG
      if(NO.gt.0) then
C
C----   Print heading
        call ABJECT   (NO)
        call STARER   (NO)
        if(IC.eq.1) then
          write (NO,100)
  100     format(' ','P . R . D .   T e r m s   f o r ',
     $               '  B a c k g r o u n d   C a l c u l a t i o n')
        else if(IC.eq.2) then
          write (NO,101)
  101     format(' ','P . R . D .   f o r   L . S . F .   a n d ',
     $               '  I n t e n s i t y   C a l c u l a t i o n s')
        else
          write (MSSLIN(1),102) IC
  102     format('IC =',I12,', which is not 1 or 2.')
          call HALT   ('OSCAR', 1)
        end if
        call STARER   (NO)
        call LINER    (3, NO)
        write (NO,103) IU,IL,CORE
  103   format(' ','Partial Redistribution Computations for the',I3,
     $             '/',I2,' Line at',1PE20.12,' Angstroms')
        if(IE.eq.1) then
C----     Print explanation
          call LUSTER (NO, IPRDD, IPRDF)
        end if
C----   Print data
        call LINER    (1, NO)
        call VELCRO   (NO, IU, IL, ICE, GMMA, XC, XP, XR, dummy, dummy)
C
      end if
C     !END
      call BYE ('OSCAR')
C
      return
      end
