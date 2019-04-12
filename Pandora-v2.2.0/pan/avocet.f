      subroutine AVOCET
     $(XLB1,QELSM,NO)
C
C     Rudolf Loeser, 1985 Jun 19
C---- Prints blended line components.
C     !DASH
      save
C     !DASH
      real*8 XLB1, dummy
      integer I, IL, IU, KSET, LDL, LINES, MMCDL, MMCRD, MMCSK, MMCVW,
     $        MMDDL, MMDWN, MMLAM, MXLNS, NO, NT
      logical KILROY
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(31),MMCDL)
      equivalence (MML(34),MMDDL)
      equivalence (MML(55),MMDWN)
      equivalence (MML( 7),MMCRD)
      equivalence (MML( 8),MMCVW)
      equivalence (MML( 9),MMCSK)
C     !EJECT
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
      equivalence (LINKDS(12),LDL  )
C     !DASH
C     !EJECT
      external PET, ACTEVO, LIDGET, ABJECT, LINER, GRIZZLY, HI, BYE
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      data MXLNS  /60/
      data KILROY /.true./
C
      call HI ('AVOCET')
C     !BEG
      if(NT.gt.0) then
C
        do 101 I = 1,NT
          call PET          (I)
          if(LDL.gt.1) then
C
            if(KILROY) then
              KILROY = .false.
              LINES = 0
C
              call ACTEVO   (QELSM, NO, LINES)
C
              write (NO,100)
  100         format(' ','Trans.',5X,'Wavelength (A)',10X,'DDL',10X,
     $                   'Wavenumber (/cm)',9X,'DWN',11X,'CDL',7X,
     $                   'CRD',9X,'CVW',9X,'CSK')
              LINES = LINES+1
              KSET  = 0
            end if
C
            call LIDGET     (XLB1, 1, dummy, 0, dummy, 0, I)
C
            if(KSET.gt.0) then
              if((LINES+LDL+1).gt.MXLNS) then
                call ABJECT (NO)
                write (NO,100)
                LINES = 1
                KSET  = 0
              end if
            end if
C
            call GRIZZLY    (NO, IU, IL, LDL, XLB1(MMDDL),
     $                       XLB1(MMDWN), XLB1(MMCDL), XLB1(MMCRD),
     $                       XLB1(MMCVW), XLB1(MMCSK), XLB1(MMLAM))
            LINES = LINES+LDL+3
            KSET  = KSET+1
          end if
  101   continue
C
      end if
C     !END
      call BYE ('AVOCET')
C
      return
      end
