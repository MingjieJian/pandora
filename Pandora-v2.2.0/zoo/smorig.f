      subroutine SMORIG
     $(POLD,PNEW,PINT,N,W,ITMX,CRIT,ITER)
C     Rudolf Loeser, 1985 Apr 02
C---- Smoothing routine -
C     Computes new values for P(i), 2 .lt. i .lt. (N-1).
C
C     The smoothing process is iterated, and the actual
C     number of iterations done is reported in ITER.
C     PINT is working storage.
C
C
C     If N .lt. 5, then PNEW=POLD.
C
C---- Recommended parameter values:
C     W = .75, ITMX = 20, and CRIT = 1.D-4.
C     !DASH
      save
C     !DASH
      real*8 CRIT, OMW, ONE, PINT, PNEW, POLD, W
      integer I, ITER, ITMX, J, KNT, KODE, N
C     !DASH
      external SMOOCH, ZEROD, MOVED, COMPD
C
C               POLD(N), PNEW(N), PINT(N)
      dimension POLD(*), PNEW(*), PINT(*)
C
      data ONE /1.D0/
C     !EJECT
C
C     !BEG
      if(N.gt.0) then
        if(N.lt.5) then
          call MOVED        (POLD,1,N,PNEW,1,N)
        else
          PNEW(  1) = POLD(  1)
          PNEW(  2) = POLD(  2)
          PNEW(N-1) = POLD(N-1)
          PNEW(N  ) = POLD(N  )
          if(N.gt.4) then
            call MOVED      (POLD,1,N,PINT,1,N)
            call ZEROD      (PNEW(3),1,(N-4))
            do 102 J = 1,ITMX
              ITER = J
              KNT  = 0
              do 100 I = 3,(N-2)
                call SMOOCH (PINT(I-2),PINT(I-1),PINT(I),
     $                       PINT(I+1),PINT(I+2),PNEW(I))
                call COMPD  (PNEW(I),PINT(I),CRIT,KODE)
                if(KODE.ne.0) then
                  KNT = KNT+1
                end if
  100         continue
              if(KNT.eq.0) then
                goto 103
              end if
              OMW = ONE-W
              do 101 I = 3,(N-2)
                PINT(I) = W*PNEW(I)+OMW*PINT(I)
  101         continue
  102       continue
          end if
        end if
      end if
  103 continue
C     !END
C
      return
      end
