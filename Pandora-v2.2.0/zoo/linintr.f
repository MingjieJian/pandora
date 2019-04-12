      subroutine LININTR
     $(XT,INCX,FT,INCF,NT,X,F,MODE,KODE,IRET)
C
C     Rudolf Loeser, 1991 Jan 30.
C---- Linear Inter(Extra)polation, for real*4 operands.
C
C---- Given the table "XT" (length "NT") and the corresponding
C     table "FT" of values of a function of XT. The values of
C     XT must all be distinct, and must be arranged in ascending
C     algebraic order, i.e. XT(J+1) .gt. XT(J), 1 .le. J .lt. NT.
C
C---- Successive elements of "XT" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "XT" lives in XT(II), where II=1+INCX*(I-1).
C
C---- Successive elements of "FT" are stored in memory locations
C     separated by the constant stride INCF, INCF > 0, such that the
C     I'th element of "FT" lives in FT(II), where II=1+INCF*(I-1).
C
C---- Given the arbitrary value X.
C
C---- This routine obtains the value of "F," the value of FT(X),
C     by linear interpolation or extrapolation.
C     The process is controlled by two switches, "MODE" and "KODE:"
C     MODE=1 means - inter(extra)polate in FT vs. XT;
C     MODE=2 means - inter(extra)polate in log(FT) vs. XT;
C     MODE=3 means - inter(extra)polate in log(FT) vs. log(XT);
C     MODE=4 means - inter(extra)polate in log10(FT) vs. XT;
C     MODE=5 means - inter(extra)polate in log10(FT) vs. log10(XT);
C     KODE=1 means - do not extrapolate;
C     KODE=2 means - extrapolate when X lies outside the range of XT.
C
C---- Upon return, status information is returned in "IRET:"
C     IRET=1 means - XT(1) .le. X .le. XT(NT), and F was obtained by
C            by interpolation;
C     IRET=2 means - NT .le. 1 and F=FT(1);
C     IRET=3 means - X .lt. XT(1), and if
C            KODE=1, then F=FT(1), but if
C            KODE=2, then F was obtained by extrapolation;
C     IRET=4 means - X .gt. XT(NT), and if
C            KODE=1, then F=FT(NT), but if
C            KODE=2, then F was obtained by extrapolation.
C
C     (This is version 2 of LINTX.)
C     !DASH
      save
C     !DASH
      real*4 F, FT, X, XT
      integer INCF, INCX, IRET, K, KF, KODE, KX, L, MODE, NOTE, NT, NTF,
     $        NTX
C     !DASH
      external SEARCHR, LINTR, ABORT
C
      dimension XT(*), FT(*)
C     !EJECT
C
C     !BEG
      if(NT.gt.1) then
        NTX = 1+INCX*(NT-1)
        NTF = 1+INCF*(NT-1)
        call SEARCHR   (XT,INCX,NT,X,K,NOTE,L)
        if(L.eq.1) then
          IRET = 1
          KX = 1+INCX*(K-1)
          KF = 1+INCF*(K-1)
          if(NOTE.eq.1) then
            F = FT(KF)
          else if(NOTE.eq.2) then
            call LINTR (XT(KX),FT(KF),XT(KX+INCX),FT(KF+INCF),MODE,X,F)
          else
            write (*,100) 'NOTE', NOTE
  100       format(' ','Error in LININTR:  ',A,' =',I12)
            call ABORT
          end if
        else if(L.eq.2) then
          IRET = 1
          F = FT(NTF)
        else if(L.eq.3) then
          IRET = 4
          if(KODE.eq.1) then
            F = FT(NTF)
          else if(KODE.eq.2) then
            call LINTR (XT(NTX-INCX),FT(NTF-INCF),XT(NTX),FT(NTF),MODE,
     $                  X,F)
          else
            write (*,100) 'KODE/1', KODE
            call ABORT
          end if
        else if(L.eq.4) then
          IRET = 3
          if(KODE.eq.1) then
            F = FT(1)
          else if(KODE.eq.2) then
            call LINTR (XT(1),FT(1),XT(1+INCX),FT(1+INCF),MODE,X,F)
          else
            write (*,100) 'KODE/2', KODE
            call ABORT
          end if
        else
          write (*,100) 'L', L
          call ABORT
        end if
      else
        IRET = 2
        F = FT(1)
      end if
C     !END
C
      return
      end
