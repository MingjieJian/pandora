      subroutine XROTOR
     $(X,W,INDEX,N,DET, FACTOR,KODE,NO,TITLE)
C
C     Rudolf Loeser, 1990 Dec 11
C
C---- Determinant calculation.
C     (This is version 2 of ROTOR.)
C
C     X is a matrix of size N x N, stored columnwise.
C     TITLE contains a description of the matrix which may be
C     written to unit NO, or to unit LUEO as part of an error message.
C
C     Returns the ( scaled ? ) determinant in DET,
C     ( and the log of the associated scale factor in FACTOR ? ).
C
C---- Upon return,
C     KODE = 1 means: everything seems ok;
C          = 0 means: something went wrong, and the result
C                     can not be trusted.
C
C     Old version of ROTOR, using scaling; saved 2006 Sep 06
C
C     !DASH
      save
C     !DASH
      real*8 DET, FACTOR, TIME, TIN, TOUT, W, X, ZERO
      integer IN, INDEX, IS, IVV, IXX, KODE, LUEO, MOX, N, NN, NO
      logical OK
      character TITLE*(*)
C     !COM
C---- XMATRIX      as of 1998 Mar 01
C
C     Old version of MATRIX, using scaling; saved 2006 Sep 06
C
      integer     PRNSW,EDJSW,KNTIN,KNTED,SCALE,ITMXS,SCALD
      real*8      CRITJ,TIMIN,TIMED
      common      /XMATRIX1/ PRNSW,EDJSW,KNTIN,KNTED,SCALE,ITMXS,SCALD
      common      /XMATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     SCALE = 1: use scaling for matrix inversion; = 0: do not.
C     ITMXS      scale factor calculation iteration limit for SCALE.
C     SCALD = 1: use scaling for determinant; = 0: do not.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external XSIGRID, MARGOT, ARROUT, SECOND, CLEANUP, DETERM, DEFACT,
     $         MOVE1, MESHED, MASHED, LINER, PRAY, WGIVE, HI, BYE
C
      dimension W(*)
C
C               X(N,N), INDEX(N)
      dimension X(*),   INDEX(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IVV   ),(IN( 2),IXX   )
C     !EJECT
C
      call HI ('XROTOR')
C     !BEG
      NN    = N*N
      KNTED = KNTED+1
      KODE  = 1
C
C     (Get, and allocate, W allotment)
      call XSIGRID     (IN, IS, MOX, 'ROTOR', N)
C
C---- Record matrix elements range data [ ? ]
      call MARGOT      (X, N, TITLE)
C---- Make copy of matrix
      call MOVE1       (X, NN, W(IXX))
C
      FACTOR = ZERO
C---- Check for special cases
      if(N.le.0) then
        KODE = 0
      else if (N.eq.1) then
        if(X(1).eq.ZERO) then
          KODE = 0
        else
          DET = X(1)
        end if
      else
C----   General case
        call SECOND    (TIN)
        if(EDJSW.gt.0) then
C----     Edit out "junk"
          call CLEANUP (X, N, N, CRITJ)
        end if
        if(SCALD.gt.0) then
C----     Scaling
          call DEFACT  (W(IXX), N, FACTOR, OK)
          if(.not.OK) goto 100
        end if
C----   Compute Determinant
        call DETERM    (W(IXX), N, N, DET, KODE, INDEX, W(IVV))
        OK = KODE.eq.1
C
        call SECOND    (TOUT)
      end if
C     !EJECT
  100 continue
      if(OK) then
        TIME  = TOUT-TIN
        TIMED = TIMED+TIME
C
        if((PRNSW.gt.0).and.(NO.gt.0)) then
          call LINER  (1, NO)
          write (NO,101) N,TIME,SCALD,EDJSW,CRITJ,FACTOR,DET,TITLE
  101     format(' ','Determinant: N',I4,2X,'Time',F8.3,' sec.; ',
     $               'SCALE',I2,2X,'EDJSW',I2,2X,'CRITJ',1PE8.1,2X,
     $               'log(Factor)',E12.4,5X,'Determinant',E16.8/
     $           ' ',A)
        end if
      else
C
        call MESHED   ('ROTOR', 2)
        write (LUEO,102) KODE,N,DET,TITLE
  102   format(' ','Determinant calculation failed.',20X,'KODE =',I5,
     $             ', N =',I5,', Determinant',1PE10.2/
     $         ' ',A)
        call ARROUT   (LUEO, X, N, N, 'Original matrix')
        call MASHED   ('ROTOR')
C
        if(SCALD.gt.0) then
          call MOVE1  (X, NN, W(IXX))
          call DEFACT (W(IXX), N, FACTOR, OK)
          call LINER  (2, LUEO)
          write (LUEO,103) FACTOR,OK
  103     format(' ','Scaled matrix',20X,'[ log(Factor) =',
     $               1PE24.16,', OK =',L10,' ]')
          call PRAY   (LUEO, W(IXX), N, N)
        end if
C
        DET    = ZERO
        FACTOR = ZERO
        KODE   = 0
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'ROTOR')
C     !END
      call BYE ('XROTOR')
C
      return
      end
