      subroutine XMOTOR
     $(XM,N,TITLE,W,IW,KODE)
C
C     Rudolf Loeser, 1990 Dec 11
C
C---- Matrix inversion.
C     (This is version 2 of MOTOR.)
C
C     XM is a matrix of size N x N; the inverse of XM will replace XM.
C     TITLE contains a description of the matrix which may be written
C     to unit LUEO as part of a message.
C
C---- Upon return,
C     KODE = 1 means: everything seems ok;
C          = 0 means: something went wrong, and the result is bad.
C
C     Old version of MOTOR, using scaling; saved 2006 Sep 06
C
C     !DASH
      save
C     !DASH
      real*8 DET, ONE, TIME, TIN, TOUT, W, XM, ZERO
      integer IN, INDEX, IS, ISCALC, ISCALR, ITEMP, IVEC, IW, IWS, JN,
     $        KODE, LUEO, MOX, MUX, N
      logical PRINT
      character TITLE*(*), qummy*8
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !EJECT
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external XINGRID, MARGOT, KOTOR, SCALMAT, SCALINV, SCALDET, WGIVE,
     $         SECOND, INVERS, CLEANUP, FISCAL, MESHED, MASHED, IGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               XM(N,N)
      dimension XM(N,*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ISCALC),(IN( 2),ISCALR),(IN( 3),IVEC  ),(IN( 4),ITEMP )
C
      dimension JN(1)
      equivalence
     $(JN( 1),INDEX )
C
      call HI ('XMOTOR')
C     !BEG
      KNTIN = KNTIN+1
      KODE  = 1
      PRINT = PRNSW.gt.0
C
C---- Record matrix elements range data [ ? ]
      call MARGOT (XM, N, TITLE)
C
C---- Check for special cases
      if(N.le.0) then
        KODE = 0
      else if (N.eq.1) then
        if(XM(1,1).eq.ZERO) then
          KODE = 0
        else
          XM(1,1) = ONE/XM(1,1)
        end if
      else
C     !EJECT
C----   General case
C       (Get, and allocate, W & IW allotments)
        call XINGRID    (IN, IS,  MOX, 'MOTOR', N)
        call KOTOR      (JN, IWS, MUX, 'MOTOR', N)
C
        call SECOND      (TIN)
        if(EDJSW.gt.0) then
C----     Edit out "junk"
          call CLEANUP   (XM, N, N, CRITJ)
        end if
        if(SCALE.gt.0) then
C----     Pre-scaling
          call FISCAL    (XM, N, N, ZZLARGE, W(ISCALC), W(ISCALR),
     $                    W(IVEC), ITMXS, 0, qummy)
          call SCALMAT   (XM, N, N, W(ISCALC), W(ISCALR), XM)
        end if
C----   Invert
        call INVERS      (XM, N, N, DET, PRINT, KODE, IW(INDEX),
     $                    W(ITEMP))
        if((KODE.gt.0).and.(SCALE.gt.0)) then
C----     Post-scaling
          call SCALINV   (XM, N, N, W(ISCALC), W(ISCALR), XM)
          if(PRINT) then
            call SCALDET (N, W(ISCALC), W(ISCALR), ZZLARGE, DET)
          end if
        end if
        call SECOND      (TOUT)
C
C       (Return W & IW allotments)
        call WGIVE       (W,  'MOTOR')
        call IGIVE       (IW, 'MOTOR')
      end if
C
      if(KODE.eq.1) then
C----   Inversion seemed to proceed properly
        TIME  = TOUT-TIN
        TIMIN = TIMIN+TIME
        if(PRINT) then
          call MESHED    ('MOTOR', 3)
          write (LUEO,100) N,TIME,SCALE,ITMXS,EDJSW,CRITJ,DET,TITLE
  100     format(' ','Matrix inversion: N',I4,2X,'Time',F8.3,
     $               ' sec.; SCALE',I2,2X,'ITMXS',I3,2X,'EDJSW',I2,2X,
     $               'CRITJ',1PE8.1,15X,'Determinant',E16.8/
     $           ' ','Label: ',A)
          call MASHED    ('MOTOR')
        end if
      else
C----   Inversion failed
        call MESHED      ('MOTOR', 3)
        write (LUEO,101) TITLE
  101   format(' ','Matrix inversion failed: ',A100)
        call MASHED      ('MOTOR')
      end if
C     !END
      call BYE ('XMOTOR')
C
      return
      end
