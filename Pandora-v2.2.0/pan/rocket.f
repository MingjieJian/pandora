      subroutine ROCKET
     $(I,IMAX,TIME,LAB)
C
C     Rudolf Loeser, 1983 May 18
C---- Composes and displays a message concerning the
C     progress of processing related to frequency summations.
C     (This is version 2 of ROCKET.)
C     !DASH
      save
C     !DASH
      real*8 AVRG, SIXTY, TG, TIME, TOGO, TOTAL, XI, ZERO
      integer I, IDEX, IL, IMAX, IU
      character BLANK*1, LAB*(*), NOTE*11
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
      equivalence (KZQ( 64),IDEX )
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
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
      intrinsic mod
C
      data SIXTY /6.D1/
C
      call HI ('ROCKET')
C     !BEG
      if(IDEX.gt.0) then
        if(I.eq.1) then
          TOTAL = ZERO
          write (NOTE,100) IDEX
  100     format('(IDEX =',I3,')')
        end if
C
        TOTAL = TOTAL+TIME
        if(mod(I,IDEX).eq.1) then
          XI   = I
          AVRG = TOTAL/XI
          TG   = IMAX-I
          TOGO = (AVRG*TG)/SIXTY
          write (*,101) LAB,IU,IL,I,IMAX,TIME,TOGO,NOTE
  101     format(' ',A3,' Trans(',I2,',',I2,'),',I4,'. Frequ. of',
     $               I4,':',1PE8.1,' sec,',E8.1,' min to go.  ',A)
          NOTE = BLANK
        end if
      end if
C     !END
      call BYE ('ROCKET')
C
      return
      end
