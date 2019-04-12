      subroutine ADAIR
     $(XA,XB,SUB,VEC,G, FEQ,DMX,DMN,NO, PD,KMAX, NSUB,GOOD)
C     Rudolf Loeser, 1986 Jul 28
C---- Computes the integral G of f(x), XA .le. x .le. XB,
C     by an adaptive trapezoidal rule procedure.
C     F(x) is computed by calls to SUB (VEC,x,f(x)); upon return
C     from ADAIR, NSUB tells how many times SUB was called.
C---- Intervals are adapted to the local behavior of f(x) by
C     succesive halvings; deferred intervals are stored in a
C     pushdown list. The array PD is used for this purpose; the
C     stack will be limited to a depth of KMAX, and PD must
C     provide room for at least 6*KMAX items. Shortage of pushdown
C     storage will force acceptance of the current subintegral; this
C     is signalled by GOOD = .false.; conversely, GOOD = .true.
C     means that PD always was large enough.
C---- If the input value of NO is > 0, then (a lot of) debug details
C     will be printed on unit NO.
C---- FEQ, DMX and DMN are accuracy control parameters:
C     FEQ specifies the minimum accceptable relative agreement
C         between explicit values of f(x) and values obtained
C         by linear interpolation;
C     DMX specifies the largest acceptable subinterval size;
C     DMN specifies the smallest necessary subinterval size.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, DMN, DMX, FEQ, FL, FLIM, FLM, FM, FR, FRM, G,
     $       GG, HALF, PD, VEC, XA, XB, XL, XLM, XM, XR, XRM, ZERO
      integer KD, KMAX, NF, NO, NOUT, NSUB
      logical GOOD
C     !COM
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      external SUB, ADAPUSH, ADANEXT
C
      dimension PD(6,KMAX), VEC(*)
C
      data ZERO,HALF /0.D0, 5.D-1/
C     !EJECT
C
C     !BEG
C---- Push data for initial interval
      XM  = XA
      XRM = HALF*(XA+XB)
      XR  = XB
      call SUB     (VEC,XM ,FM )
      call SUB     (VEC,XRM,FRM)
      call SUB     (VEC,XR ,FR )
      NF = 3
      KD = 0
      call ADAPUSH (PD)
C---- Other initialization
      FLIM = FEQ
      DMAX = DMX
      DMIN = DMN
      NOUT = NO
      GG   = ZERO
      GOOD = .true.
C---- Now: go!
      call ADANEXT (SUB,VEC,PD,KMAX,GOOD)
C---- Finally, return results.
      G    = GG
      NSUB = NF
C     !END
C
      return
      end
