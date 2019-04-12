      subroutine KOMODO
     $(KOLEV,XLMT,XLMF,XLME,XLMA,XLMB,XLMR,INFSM,INLSM,WSM,N,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Prints various Lyman calculations editing parameters, for LINTEL.
C     !DASH
      save
C     !DASH
      real*8 WSM, XLMA, XLMB, XLME, XLMF, XLMR, XLMT
      integer INFSM, INLSM, JF, JL, KOLEV, N, NO
C     !DASH
      external  LINER, HI, BYE
      intrinsic max, min
C
      call HI ('KOMODO')
C     !BEG
      call LINER (1,NO)
      write (NO,100) XLMT,XLMF,XLME,XLMA,XLMB,XLMR
  100 format(' ','LMT     ',1PE12.4,14X,
     $           'EP1 editing parameters (these are explained in the ',
     $           'LYMAN printout)'/
     $       ' ','LMF     ',E12.4/
     $       ' ','LME     ',E12.4/
     $       ' ','LMA     ',E12.4/
     $       ' ','LMB     ',E12.4/
     $       ' ','LMR     ',E12.4)
C
      JF = max(1,INFSM)
      JL = min(N,INLSM)
      if(JL.le.0) then
        JL = N
      end if
C
      call LINER (1,NO)
      write (NO,101) WSM,KOLEV,JF,KOLEV,JL
  101 format(' ','WSM     ',1PE12.4,14X,'RK',I2,' smoothing parameter'/
     $       ' ','INFSM   ',I3,23X,'RK',I2,' smoothing limit indices'/
     $       ' ','INLSM   ',I3)
C     !END
      call BYE ('KOMODO')
C
      return
      end
