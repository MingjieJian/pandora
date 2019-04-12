      subroutine HARE
     $(KF,GOODT)
C
C     Rudolf Loeser, 1989 Feb 08
C---- Enforces limits on KF.
C     (This is version 7 of HARE.)
C     !DASH
      save
C     !DASH
      integer IL, IU, KF, KFL, KMMAX, KMX, LUEO, N, NKF, NKFL
      logical GOODT, LKF, LNKF
      character BLANK*1, KMMES*22
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(132),KMMAX)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
C     !DASH
C     !EJECT
      external  MESHED, MASHED, LINER, HI, BYE
      intrinsic abs
C
      data KFL,NKFL /1000, 10000/
C
C     KFL relates to the use of frequency index in the (packed)
C     DIANA/ORION Data Block name - see subroutine POD and common
C     block ARCHER (NNKODS(1)); and to a similar use in the
C     Continuum Data Block name - see subroutine BET and common block
C     KAPPA (KAKODS(3)).
C
C
      call HI ('HARE')
C     !BEG
      NKF = N*KF
      KMX = abs(KMMAX)
C
      LKF = KF.ge.KFL
      if(KMX.gt.0) then
        LKF = LKF.or.(KF.gt.KMX)
        write (KMMES,100) KMMAX
  100   format('input limit =',I9)
      else
        KMMES = BLANK
      end if
      GOODT = .not.LKF
C
      LNKF = NKF.ge.NKFL
      if(LKF.or.LNKF) then
        call MESHED ('HARE', 3)
        write (LUEO,101) IU,IL,KF,KMMES,KFL,N,NKF,NKFL
  101   format(' ',2('**********'),4X,'Transition (',I2,'/',I2,'):  ',
     $             'XI limits exceeded.',4X,2('**********')//
     $         ' ','KF =',I12,6X,A22,10X,'mandatory KF limit =',I14/
     $         ' ','N  =',I12,10X,'N*KF =',I12,10X,'suggested N*KF ',
     $             'limit =',I12)
        call LINER  (1, LUEO)
        write (LUEO,102) IU,IL
  102   format(' ','Note that the amount of computing for transition ',
     $             '(',I2,'/',I2,') depends directly on N*KF.')
        if(.not.GOODT) then
          write (LUEO,103)
  103     format(' ','***** This run will be stopped.')
        end if
        call MASHED ('HARE')
      end if
C     !END
      call BYE ('HARE')
C
      return
      end
