      subroutine ROTOR
     $(XM,N,TITLE,NO,W,IW,DET,KODE)
C
C     Rudolf Loeser, 1990 Dec 11
C
C---- Determinant calculation.
C     (This is version 2 of ROTOR.)
C
C     XM is a matrix of size N x N, stored columnwise.
C     TITLE contains a description of the matrix which may be
C     written to unit NO, or to unit LUEO as part of an error message.
C
C     Returns the determinant in DET.
C
C---- Upon return,
C     KODE = 1 means: everything seems ok;
C          = 0 means: something went wrong, and the result
C                     can not be trusted.
C     !DASH
      save
C     !DASH
      real*8 DET, TIME, TIN, TOUT, W, XM, ZERO
      integer IN, INDEX, IS, IVV, IW, IWS, IXX, JN, KODE, LUEO, MOX,
     $        MUX, N, NN, NO
      character TITLE*(*)
C     !COM
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external SIGRID, MARGOT, ARROUT, SECOND, CLEANUP, DETERM, MESHED,
     $         MOVE1, KOTOR, MASHED, LINER, PRAY, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XM(N,N)
      dimension XM(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IVV   ),(IN( 2),IXX   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),INDEX )
C
      call HI ('ROTOR')
C     !BEG
      NN    = N*N
      KNTED = KNTED+1
      KODE  = 1
C
C     (Get, and allocate, W & IW allotments)
      call SIGRID      (IN, IS,  MOX, 'ROTOR', N)
      call KOTOR       (JN, IWS, MUX, 'ROTOR', N)
C
C---- Record matrix elements range data [ ? ]
      call MARGOT      (XM, N, TITLE)
C---- Make copy of matrix
      call MOVE1       (XM, NN, W(IXX))
C
C---- Check for special cases
      if(N.le.0) then
        KODE = 0
      else if (N.eq.1) then
        if(XM(1).eq.ZERO) then
          KODE = 0
        else
          DET = XM(1)
        end if
      else
C     !EJECT
C----   General case
        call SECOND    (TIN)
C
        if(EDJSW.gt.0) then
C----     Edit out "junk"
          call CLEANUP (XM, N, N, CRITJ)
        end if
C
C----   Compute Determinant
        call DETERM    (XM, N, N, DET, KODE, IW(INDEX), W(IVV))
C
        call SECOND    (TOUT)
      end if
      if(KODE.eq.1) then
        TIME  = TOUT-TIN
        TIMED = TIMED+TIME
C
        if((PRNSW.gt.0).and.(NO.gt.0)) then
          call LINER   (1, NO)
          write (NO,100) N,TIME,EDJSW,CRITJ,DET,TITLE
  100     format(' ','Determinant: N',I4,2X,'Time',F8.3,' sec.; ',
     $               'EDJSW',I2,2X,'CRITJ',1PE8.1,5X,
     $               'Determinant',E16.8/
     $           ' ',A)
        end if
      else
C
        call MESHED    ('ROTOR', 2)
        write (LUEO,101) KODE,N,DET,TITLE
  101   format(' ','Determinant calculation failed.',20X,'KODE =',I5,
     $             ', N =',I5,', Determinant',1PE10.2/
     $         ' ',A)
        call ARROUT    (LUEO, W(IXX), N, N, 'Original matrix')
        call MASHED    ('ROTOR')
C
        DET    = ZERO
        KODE   = 0
      end if
C
C     (Give back W & IW allotments)
      call WGIVE       (W,  'ROTOR')
      call IGIVE       (IW, 'ROTOR')
C     !END
      call BYE ('ROTOR')
C
      return
      end
