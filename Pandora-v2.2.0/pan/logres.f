      subroutine LOGRES
     $(CEK,N,NLM,QHI,NT)
C
C     Rudolf Loeser, 1980 Mar 14
C---- Saves CHECKS, for iterative summary.
C     !DASH
      save
C     !DASH
      real*8 CEK, QHI
      integer IL, IQICK, IQQHR, IU, J, JUL, KCEK, KCHI, KLIN, N, NLM,
     $        NT
      logical FIRST
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
      equivalence (IQQ(323),IQQHR)
      equivalence (IQQ(302),IQICK)
C     !DASH
      external PET, INTRANS, BERWYN, HI, BYE
C
C               CEK(N,NLM), QHI(N,NT)
      dimension CEK(N,*),   QHI(N,*)
C
      data KCEK,KCHI /2, 9/
C     !EJECT
C
      call HI ('LOGRES')
C     !BEG
      if((NLM.gt.0).and.(IQICK.gt.0)) then
        FIRST = .true.
        do 100 J = 1,NLM
          call BERWYN    (KCEK, 'Logres', 'Check', (J+2), 0, CEK(1,J),
     $                    N, FIRST)
          FIRST = .false.
  100   continue
      end if
C
      if(IQQHR.gt.0) then
        FIRST = .true.
        do 101 J = 1,NT
          call PET       (J)
          if(KLIN.eq.1) then
            call INTRANS (IU, IL, 'LOGRES', JUL)
            call BERWYN  (KCHI, 'Logres', 'Chi', IU, IL, QHI(1,JUL),
     $                    N, FIRST)
            FIRST = .false.
          end if
  101   continue
      end if
C     !END
      call BYE ('LOGRES')
C
      return
      end
