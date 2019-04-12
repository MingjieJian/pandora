      subroutine BOHUN
     $(NO,N,NT,NDW,TE,V,DW,VM,VR)
C
C     Rudolf Loeser, 1982 Dec 23
C---- Prints Doppler Widths data.
C     !DASH
      save
C     !DASH
      real*8 DW, TE, V, VM, VR
      integer I, IE, IL, IPRO, IQVSW, IS, IU, J, KLIN, KOUNT, LSFP, N,
     $        NDW, NO, NT
      logical VSWITCH
      character LABS*15
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
      equivalence (LINKDS(14),LSFP )
      equivalence (LINKDS( 5),IPRO )
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
      equivalence (IQQ(173),IQVSW)
C     !DASH
C     !EJECT
      external  WACE, BUDE, LINER, PET, HI, BYE
      intrinsic min
C
C               TE(N), V(N), VR(N), VM(N), DW(N,NT)
      dimension TE(*), V(*), VR(*), VM(*), DW(N,*)
C
      dimension LABS(7)
C
      call HI ('BOHUN')
C     !BEG
      VSWITCH = IQVSW.gt.0
      call WACE    (NO, NDW, VSWITCH)
      call LINER   (1, NO)
C
      IE = 0
  100 continue
        IS = IE+1
        IE = min((IE+7),N)
        call BUDE  (IS, IE, NDW, LABS, KOUNT)
        call LINER (1, NO)
C
        write (NO,101) (LABS(I),I=1,KOUNT)
  101   format(' ',11X,1X,7(1X,A15))
        call LINER (1,NO)
        write (NO,102) (TE(I),I=IS,IE)
  102   format(' ','     TE (K)',1P7E16.8)
        write (NO,103) (V(I),I=IS,IE)
  103   format(' ',' V (km/sec)',1P7E16.8)
        if(VSWITCH) then
          write (NO,104) (VR(I),I=IS,IE)
  104     format(' ','VR (km/sec)',1P7E16.8)
          write (NO,105) (VM(I),I=IS,IE)
  105     format(' ','VM (km/sec)',1P7E16.8)
        end if
C
        do 107 J = 1,NT
          call PET (J)
          if((KLIN.eq.1).or.(KLIN.eq.2)) then
            if((LSFP.gt.0).or.(IPRO.gt.0)) then
              write (NO,106) IU,IL,(DW(I,J),I=IS,IE)
  106         format(' ','  DW(',I2,',',I2,')',1P7E16.8)
            end if
          end if
  107   continue
C
      if(IE.lt.N) goto 100
C     !END
      call BYE ('BOHUN')
C
      return
      end
