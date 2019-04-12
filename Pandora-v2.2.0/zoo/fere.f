      subroutine FERE
     $(XO,INCXO,FO,INCFO,NO,XN,INCXN,FN,INCFN,NN,MODE,KODE)
C     Rudolf Loeser, 1987 Sep 18
C
C---- Linear inter(extra)polation.
C
C---- Given
C     an 'old' table of abscissae, XO(i), 1 .le. i .le. NO, and
C     a corresponding table of ordinates, FO(i), 1 .le. i .le. NO;
C     and given
C     a 'new' table of abscissae, XN(i), 1 .le. i .le. NN.
C
C---- Successive elements of "XO" are stored in memory locations
C     separated by the constant stride INCXO, INCXO > 0, such that the
C     I'th element of "XO" lives in XO(II), where II=1+INCXO*(I-1).
C
C---- Successive elements of "FO" are stored in memory locations
C     separated by the constant stride INCFO, INCFO > 0, such that the
C     I'th element of "FO" lives in FO(II), where II=1+INCFO*(I-1).
C
C---- "FERE" computes a table of ordinates, FN(i), 1 .le. i .le. NN,
C     corresponding to the 'new' table of abscissae, using "LININT".
C     MODE=1 means: use XT and FT as is, i.e. lin-lin;
C     MODE=2 means: use log(XT), and FT as is, i.e. semilog;
C     MODE=3 means: use log(XT) and log(FT), i.e. log-log.
C     KODE=2 means: extrapolate beyond the range of XT; =1 means: don't.
C
C---- Successive elements of "XN" are stored in memory locations
C     separated by the constant stride INCXN, INCXN > 0, such that the
C     I'th element of "XN" lives in XN(II), where II=1+INCXN*(I-1).
C
C---- Successive elements of "FN" are stored in memory locations
C     separated by the constant stride INCFN, INC > 0, such that the
C     I'th element of "FN" lives in FN(II), where II=1+INCFN*(I-1).
C
C---- The table XO must be in ascending algebraic order,
C     i.e. XO(i) .gt. XO(i-1), 2 .le. i .le. NO.
C     !DASH
      save
C     !DASH
      real*8 FN, FO, XN, XO
      integer I, IFN, INCFN, INCFO, INCXN, INCXO, IRET, IXN, JS, KODE,
     $        MODE, NN, NO
C     !DASH
      external LININT
C
      dimension XN(*), FN(*), XO(*), FO(*)
C     !EJECT
C
C     !BEG
      JS  = 0
      IXN = 1-INCXN
      IFN = 1-INCFN
      do 100 I = 1,NN
        IXN = IXN+INCXN
        IFN = IFN+INCFN
        call LININT (XO,INCXO,FO,INCFO,NO,XN(IXN),FN(IFN),
     $               MODE,KODE,IRET)
  100 continue
C     !END
C
      return
      end
