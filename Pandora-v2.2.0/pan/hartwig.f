      subroutine HARTWIG
     $(LININT,LINFLX,LINK,BACKGR,LU)
C
C     Rudolf Loeser, 1994 Dec 07
C---- Prints a header, for SIAM.
C
C     LINK = 1 means: LSF-related
C            2        PRD-related
C            3        line-free
C
C---- At present, LINK=1 and LINK=2 are not distinguished;
C     they mean the same thing: LSF background.  2006 Mar 15
C
C     !DASH
      save
C     !DASH
      integer IL, IQPCP, IU, LINK, LU, NO
      logical BACKGR, LINFLX, LININT
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(329),IQPCP)
C     !DASH
      external PRIAM, LINER, ABJECT, ZEUS, HI, BYE
C
      call HI ('HARTWIG')
C     !BEG
      if(BACKGR) then
        LU = NO
        call PRIAM (LU, 'CONTINUUM', 9)
        goto 108
      end if
C
      if(LININT.or.LINFLX) then
        call ZEUS       (NO, IQPCP, LU)
        if(LU.gt.0) then
          if(LINK.eq.3) then
            call ABJECT (LU)
          else
            call LINER  (2, LU)
          end if
C     !EJECT
          if(LININT.and.LINFLX) then
            write (LU,100) 'intensity & flux',IU,IL
  100       format(' ','Line-specific background ',A,' calculations ',
     $                 'for transition ',I2,'/',I2)
          else if(LININT) then
            write (LU,100) 'intensity',IU,IL
          else
            write (LU,100) 'flux',IU,IL
          end if
C
          call LINER    (2, LU)
          if(LINK.eq.1) then
            write (LU,101)
  101       format(' ','These background calculations use the ',
     $                 'results from a regular continuum ',
     $                 'calculation.')
          else if(LINK.eq.2) then
            write (LU,101)
C 102       format(' ','...
          else if(LINK.eq.3) then
            write (LU,103)
  103       format(' ','These background calculations use the ',
     $                 'results from a line-free continuum '
     $                 'calculation.')
          end if
          write (NO,104)
  104     format(' ','(See Section BACKGROUND, near the beginning of ',
     $               'this output file; note also option PROCPRNT).')
          call LINER    (2, LU)
          if(LINK.eq.3) then
            write (LU,105)
  105       format(' ','These line-free results are used to compute ',
     $                 'the  R E S I D U A L S, below.')
          else
            write (LU,106)
  106       format(' ','These background results are computed and ',
     $                 'printed for  I N F O R M A T I O N  only.')
          end if
          call LINER    (2, LU)
        end if
      else
        call LINER      (1, NO)
        write (NO, 107)
  107   format(' ','To see line-specific background intensities, turn ',
     $             'option PROCPRNT on.')
      end if
C
  108 continue
C     !END
      call BYE ('HARTWIG')
C
      return
      end
