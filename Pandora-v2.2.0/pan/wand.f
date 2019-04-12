      subroutine WAND
     $(LURR,XLB1,FCE,PCE)
C
C     Rudolf LOESER, 1980 Jan 04
C---- Puts "transition" data into restart file.
C     !DASH
      save
C     !DASH
      real*8 FCE, ONE, PCE, XLB1, ZERO, dummy
      integer I, IL, IQCEF, IQESW, IU, IUL, KLIN, LURR, METSE, MMAW,
     $        MMJBR, MMQHI, MMRHO, MMRHW, N, NT
      logical METSW, OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
      equivalence (JZQ( 1),N  )
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
      equivalence (LINKDS( 6),METSE)
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(28),MMRHO)
      equivalence (MML(29),MMRHW)
      equivalence (MML(27),MMJBR)
      equivalence (MML(66),MMQHI)
      equivalence (MML(65),MMAW )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(328),IQCEF)
      equivalence (IQQ(186),IQESW)
C     !DASH
C     !EJECT
      external LIDGET, PENT, PET, INDXNT, KONSTD, HI, BYE
C
C               XLB1(Li1len), FCE(N,NT), PCE(NT)
      dimension XLB1(*),      FCE(N,*),  PCE(*)
C
      call HI ('WAND')
C     !BEG
      METSW = IQESW.gt.0
      do 102 I = 1,NT
        call PET        (I)
        if(KLIN.eq.1) then
          call LIDGET   (XLB1, 1, dummy, 0, dummy, 0, I)
          call INDXNT   (IU, IL, OK, IUL)
C
          if(METSW) then
            write (LURR,100) IU,IL,METSE
  100       format('METSE ',2I4,' ( ',I1,' ) > ')
          end if
C
          call PENT     (LURR, XLB1(MMRHO), IU, IL, 'RHO' )
          call PENT     (LURR, XLB1(MMRHW), IU, IL, 'RHWT')
          call PENT     (LURR, XLB1(MMJBR), IU, IL, 'JBAR')
          call PENT     (LURR, XLB1(MMQHI), IU, IL, 'CHI' )
          call PENT     (LURR, XLB1(MMAW ), IU, IL, 'AW'  )
C
          if(IQCEF.gt.0) then
            if(PCE(IUL).ne.ZERO) then
              write (LURR,101) IU,IL,PCE(IUL)
  101         format('PCE ',2I4,' ( ',1PE10.2,' ) > ')
            end if
            call KONSTD (FCE(1,IUL), 1, N, ONE, OK)
            if(.not.OK) then
              call PENT (LURR, FCE(1,IUL),  IU, IL, 'FCE' )
            end if
          end if
C
        end if
  102 continue
C     !END
      call BYE ('WAND')
C
      return
      end
