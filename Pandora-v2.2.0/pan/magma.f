      subroutine MAGMA
     $(COOL,N,COOLSM,XVAL,W,IW)
C
C     Rudolf Loeser, 1998 Oct 20
C---- Makes a smoothed version of the cooling rate.
C     (This is version 2 of MAGMA.)
C     !DASH
      save
C     !DASH
      real*8 COOL, COOLSM, W, XVAL
      integer INDX, IW, N, jummy
      logical lummy
      character LABEL*100, TYPE*3
C     !DASH
      external MOVE1, INDARRD, SMOOTH, HI, BYE
C
      dimension W(*), IW(*)
C
C               COOL(N), COOLSM(N), XVAL(N)
      dimension COOL(*), COOLSM(*), XVAL(*)
C
      data LABEL /'Cooling Rate'/
      data TYPE,INDX /'lin', 0/
C
      call HI ('MAGMA')
C     !BEG
      call MOVE1   (COOL, N, COOLSM)
      call INDARRD (XVAL, 1, 1, N)
      call SMOOTH  (XVAL, COOLSM, N, TYPE, LABEL, INDX, W, IW,
     $              jummy, lummy)
C     !END
      call BYE ('MAGMA')
C
      return
      end
