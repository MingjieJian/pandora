      subroutine HEMRU
     $(N,NT,RHO,YBR,QHI,AW)
C
C     Rudolf Loeser, 2006 Jun 13
C---- Fiddles with input RHO, JBAR, etc, when OPTHINL = on.
C     !DASH
      save
C     !DASH
      real*8 AW, QHI, RHO, YBR
      integer I, IL, IQOTL, IU, IUL, KLIN, N, NT
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
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
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
      equivalence (IQQ(344),IQOTL)
C     !DASH
      external PET, INTRANS, ONE1, ZERO1, HI, BYE
C
C               RHO(N,NT), YBR(N,NT), QHI(N,NT), AW(N,NT)
      dimension RHO(N,*),  YBR(N,*),  QHI(N,*),  AW(N,*)
C
      call HI ('HEMRU')
C     !BEG
      if(IQOTL.gt.0) then
        do 100 I = 1,NT
          call PET       (I)
          if(KLIN.eq.3) then
            call INTRANS (IU, IL, 'HEMRU', IUL)
            call ONE1    (RHO(1,IUL), N)
            call ZERO1   (YBR(1,IUL), N)
            call ZERO1   (QHI(1,IUL), N)
            call ZERO1   ( AW(1,IUL), N)
          end if
  100   continue
      end if
C     !END
      call BYE ('HEMRU')
C
      return
      end
