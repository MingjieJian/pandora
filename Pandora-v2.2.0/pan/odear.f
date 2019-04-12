      subroutine ODEAR
     $(XA,XB, SAVE, F,G, PD,GOOD,EST,CRT)
C
C     Rudolf Loeser, 1991 Dec 17
C
C     (This is a specific adaptation of the ADAIR package!)
C
C---- Computes the integral F of f(x)*g(x) and G of g(x),
C     over the range XA .le. x .le. XB,
C     by an adaptive trapezoidal rule procedure.
C
C---- Intervals are adapted to the local behavior of f(x)*g(x)
C     by succesive halvings; deferred intervals are stored in a
C     pushdown list. The array PD is used for this purpose; the
C     stack will be limited to a depth of KMAX (in ESEL), and PD must
C     provide room for at least 9*KMAX items. Shortage of pushdown
C     storage will force acceptance of the current subintegral; this
C     is signalled by GOOD = .false.; conversely, GOOD = .true.
C     means that PD always was large enough.
C
C---- SBFQ, SBMX and SBMN (in ESEL) are accuracy control parameters:
C     SBFQ  specifies the minimum accceptable relative agreement
C           between explicit values of f(x) and values obtained
C           by linear interpolation;
C     SBMX  specifies the largest acceptable subinterval size;
C     SBMN  specifies the smallest necessary subinterval size.
C
C---- SAVE is an array in which values of x, f, g, etc., are
C     saved for later uses; SAVE contains room for NMAX (in ESEL)
C     such sets. Upon return, NS (in ESEL) tells how many
C     sets were saved.
C
C---- EST is the computed initial estimate of the integral, and
C     CRT is the computed subintegral significance criterion.
C     !DASH
      save
C     !DASH
      real*4 CRT, EST, F, FF, FL, FLM, FM, FOURTH, FR, FRM, G, GG, GL,
     $       GLM, GM, GR, GRM, HALF, PD, SAVE, THSND, TWO, XA, XB, XL,
     $       XLM, XM, XR, XRM, ZERO
      integer KD
      logical GOOD
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
C     !EJECT
      external  ODEPUSH, ODENEXT, ENTE, HI, BYE
      intrinsic abs
C
C               PD(9,KMAX), SAVE(NMAX,4)
      dimension PD(*),      SAVE(*)
C
      data ZERO, FOURTH, HALF /0.E0, 2.5E-1, 5.E-1/
      data TWO, THSND /2.E0, 1.E3/
C
      call HI ('ODEAR')
C     !BEG
C---- Push data for initial interval
      XM  = XA
      XRM = HALF*(XA+XB)
      XR  = XB
      call ENTE (XM,  SAVE, FM,  GM )
      call ENTE (XRM, SAVE, FRM, GRM)
      call ENTE (XR,  SAVE, FR,  GR )
      KD = 0
      call ODEPUSH (PD)
C---- Other initialization
      FF   = ZERO
      GG   = ZERO
      GOOD = .true.
      EST  = abs(XB-XA)*FOURTH*(FM+TWO*FRM+FR)
      CRT  = abs((EST/THSND)*SBFQ)
C---- Now: go!
      call ODENEXT (SAVE, PD, CRT, GOOD)
C---- Finally, return results.
      F = FF
      G = GG
C     !END
      call BYE ('ODEAR')
C
      return
      end
      subroutine ODENEXT
     $(SAVE,PD,CRT,GOOD)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Generates and sums a sequence of component subintegrals,
C     for ODEAR.
C     !DASH
      save
C     !DASH
      real*4 CRT, FF, FL, FLM, FM, FR, FRM, FS, GG, GL, GLM, GM, GR,
     $       GRM, GS, PD, SAVE, XL, XLM, XM, XR, XRM
      integer KD
      logical FULL, GOOD, USE
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
C     !EJECT
      external ODEPOP, ODECOMP, ODEPUSH, HI, BYE
C
C               PD(9,KMAX), SAVE(NMAX,4)
      dimension PD(*),      SAVE(*)
C
      call HI ('ODENEXT')
C     !BEG
  100 continue
      if(KD.gt.0) then
C----   Stack is not empty: get next interval to work on
        call ODEPOP      (PD)
  101   continue
C----   Compute subintegral, and test its acceptability
        call ODECOMP     (SAVE, FS, GS, CRT, USE)
        if(USE) then
C----     The subintegral is acceptable:
C         update total integral, and do next interval from stack
          FF = FF+FS
          GG = GG+GS
          goto 100
        else
C----     The subintegral is not acceptable:
C         save right half of interval, and refine left half
          FULL = KD.ge.KMAX
          if(FULL) then
C----       There is not enough room in PD for deferring the right
C           half to permit refining the left half:
C           must accept subintegral willy-nilly, but set error flag
            FF = FF+FS
            GG = GG+GS
            GOOD = .false.
            goto 100
          else
C----       Push right half-interval on stack
            call ODEPUSH (PD)
C----       Set up left half-interval as the next interval to work on
            XR = XM
            FR = FM
            GR = GM
            XM = XLM
            FM = FLM
            GM = GLM
            goto 101
          end if
        end if
      end if
C     !END
      call BYE ('ODENEXT')
C
      return
      end
      subroutine ODEPOP
     $(PD)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Pops the (non-empty) stack, for ODEAR.
C     !DASH
      save
C     !DASH
      real*4 FF, FL, FLM, FM, FR, FRM, GG, GL, GLM, GM, GR, GRM, PD, XL,
     $       XLM, XM, XR, XRM
      integer KD
C     !COM
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
      external HI, BYE
C
C               PD(9,KMAX)
      dimension PD(9,*)
C
      call HI ('ODEPOP')
C     !BEG
      XL = PD(1,KD)
      FL = PD(2,KD)
      GL = PD(3,KD)
      XM = PD(4,KD)
      FM = PD(5,KD)
      GM = PD(6,KD)
      XR = PD(7,KD)
      FR = PD(8,KD)
      GR = PD(9,KD)
C
      KD = KD-1
C     !END
      call BYE ('ODEPOP')
C
      return
      end
      subroutine ODEPUSH
     $(PD)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Pushes the stack, for ODEAR.
C     !DASH
      save
C     !DASH
      real*4 FF, FL, FLM, FM, FR, FRM, GG, GL, GLM, GM, GR, GRM, PD, XL,
     $       XLM, XM, XR, XRM
      integer KD
C     !COM
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
      external HI, BYE
C
C               PD(9,KMAX)
      dimension PD(9,*)
C
C
      call HI ('ODEPUSH')
C     !BEG
      KD = KD+1
C
      PD(1,KD) = XM
      PD(2,KD) = FM
      PD(3,KD) = GM
      PD(4,KD) = XRM
      PD(5,KD) = FRM
      PD(6,KD) = GRM
      PD(7,KD) = XR
      PD(8,KD) = FR
      PD(9,KD) = GR
C     !END
      call BYE ('ODEPUSH')
C
      return
      end
      subroutine ODECOMP
     $(SAVE,FS,GS,CRT,USE)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Computes and tests the subintegrals, FS and GS,
C     over the current subinterval (XL,XR), for ODEAR.
C     Returns with USE = .true. if the value of FS is acceptable.
C     !DASH
      save
C     !DASH
      real*4 CRT, DX, FF, FL, FLM, FM, FOURTH, FR, FRM, FS, GG, GL, GLM,
     $       GM, GR, GRM, GS, SAVE, TWO, XL, XLM, XM, XR, XRM
      integer KD
      logical MORE, USE
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
C     !EJECT
      external  ODEMORE, ODETEST, HI, BYE
      intrinsic abs
C
C               SAVE(NMAX,4)
      dimension SAVE(*)
C
      data FOURTH, TWO /2.5E-1, 2.E0/
C
      call HI ('ODECOMP')
C     !BEG
      DX = XR-XL
      FS = FOURTH*DX*(FL+TWO*FM+FR)
      GS = FOURTH*DX*(GL+TWO*GM+GR)
C
      USE  = .true.
      MORE = .false.
C
      if(abs(FS).ge.CRT) then
        if(DX.gt.SBMX) then
C----     Interval is too large, therefore
C         get supplementary data and reject FS
          call ODEMORE       (SAVE)
          MORE = .true.
          USE  = .false.
        else
          if(DX.ge.SBMN) then
C----       Current interval is not yet too small, therefore
C           perform tests on explicit vs. interpolated function values,
C           and reject FS if necessary
            call ODETEST     (FL, FM, FR, USE)
            if(USE) then
C----         OK so far: get supplementary data for further tests
              call ODEMORE   (SAVE)
              MORE = .true.
C----         Test left half
              call ODETEST   (FL, FLM, FM, USE)
              if(USE) then
C----           Still OK, so also test right half
                call ODETEST (FM, FRM, FR, USE)
              end if
            end if
          end if
        end if
      end if
      if((.not.USE).and.(.not.MORE)) then
C----   Make sure that supplementary data are available, since
C       they are needed for the next level of interval refinement
        call ODEMORE         (SAVE)
      end if
C     !END
      call BYE ('ODECOMP')
C
      return
      end
      subroutine ODEMORE
     $(SAVE)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Provides additional left- and right-subinterval
C     midpoint data, for ODEAR.
C     !DASH
      save
C     !DASH
      real*4 FF, FL, FLM, FM, FR, FRM, GG, GL, GLM, GM, GR, GRM, HALF,
     $       SAVE, XL, XLM, XM, XR, XRM
      integer KD
C     !COM
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
      external ENTE, HI, BYE
C
C               SAVE(NMAX,4)
      dimension SAVE(*)
C
      data HALF /5.E-1/
C
      call HI ('ODEMORE')
C     !BEG
      XLM = HALF*(XL+XM)
      call ENTE (XLM, SAVE, FLM, GLM)
C
      XRM = HALF*(XM+XR)
      call ENTE (XRM, SAVE, FRM, GRM)
C     !END
      call BYE ('ODEMORE')
C
      return
      end
      subroutine ODETEST
     $(ZL,ZM,ZR,USE)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Checks whether a subinterval is acceptable by testing
C     its midpoint's interpolated function value, for ODEAR.
C     Sets USE = .false. if unacceptable.
C     !DASH
      save
C     !DASH
      real*4 DZ, FF, FL, FLM, FM, FR, FRM, GG, GL, GLM, GM, GR, GRM,
     $       HALF, XL, XLM, XM, XR, XRM, ZERO, ZL, ZM, ZR, ZV
      integer KD
      logical USE
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
      common /ODECOMM/ XL,FL,GL,XLM,FLM,GLM,
     $                 XM,FM,GM,XRM,FRM,GRM,XR,FR,GR,
     $                 FF,GG,KD
C     !DASH
      external  HI, BYE
      intrinsic abs, max
C
      data ZERO, HALF /0.E0, 5.E-1/
C
      call HI ('ODETEST')
C     !BEG
      ZV = HALF*(ZL+ZR)
      DZ = max(abs(ZV),abs(ZM))
      if(DZ.ne.ZERO) then
        if(((abs(ZV-ZM))/DZ).ge.SBFQ) then
          USE = .false.
        end if
      end if
C     !END
      call BYE ('ODETEST')
C
      return
      end
