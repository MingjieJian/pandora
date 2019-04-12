      subroutine SINEW
     $(XJNU,APD,XLDT,NDT,N,RJ,TDUSTN,TDUSTO,XDT,DDT,MDTRY1,PLANK,RB,
     $ NTRY1,NTRY2,MDTRY2,TW,VEC,B,YAYB,DUMP)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Computes TDUSTN, for QUIVER.
C     Note: the tabulated values of XLDT must be descending order.
C     !DASH
      save
C     !DASH
      real*8 APD, B, DDT, PLANK, R, RB, RJ, RO, TDUSTN, TDUSTO, TRY,
     $       TRYO, TW, VEC, XDT, XJNU, XLDT, YAYB
      integer I, KODE, MDTRY1, MDTRY2, N, NDT, NTRY1, NTRY2
      logical DUMP
C     !DASH
      external AUBURN, RENARD, ZEROI, READY, AIM, MOVED, BURUNA, SWINE,
     $         BIDDLE, RIDDLE, HI, BYE
C
C               XDT(N), XLDT(N), TDUSTO(N), TDUSTN(N), RJ(N), VEC(NDT),
      dimension XDT(*), XLDT(*), TDUSTO(*), TDUSTN(*), RJ(*), VEC(*),
C
C               PLANK(N,NDT), TW(NDT), NTRY1(N), NTRY2(N), XJNU(N,NDT),
     $          PLANK(N,*),   TW(*),   NTRY1(*), NTRY2(*), XJNU(*),
C
C               B(NDT), RB(N), YAYB(NDT), APD(NDT)
     $          B(*),   RB(*), YAYB(*),   APD(*)
C     !EJECT
C
      call HI ('SINEW')
C     !BEG
C---- Compute XDT
      call AUBURN     (NDT, XLDT, XDT)
C---- Compute integration weights
      call BURUNA     (XDT, NDT, TW)
C---- Compute RJ
      call RENARD     (XJNU, APD, TW, YAYB, VEC, NDT, N, RJ)
      if(DUMP) then
        call BIDDLE   (NDT, XDT, TW)
      end if
C---- Initialize counters
      call ZEROI      (NTRY1, 1, N)
      call ZEROI      (NTRY2, 1, N)
C---- Find new dust temperature, at each depth
      do 100 I = 1,N
        if(DUMP) then
          call RIDDLE (I, RJ(I))
        end if
C----   Establish bracketing interval
        call READY    (NTRY1(I), MDTRY1, DDT, RB(I), TDUSTO(I), B, N,
     $                 XLDT, NDT, APD, TW, VEC, RJ(I), TRYO, RO, TRY,
     $                 R, KODE, DUMP)
        if(KODE.le.0) then
C----     Zero in on solution
          call AIM    (TRYO, RO, TRY, R, NTRY2(I), B, N, XLDT, NDT,
     $                 APD, TW, VEC, RB(I), RJ(I), MDTRY2, DDT, DUMP)
        end if
        TDUSTN(I) = TRY
        call MOVED    (B, 1, NDT, PLANK(I,1), N, NDT)
  100 continue
C---- Make adjsutments for REFLECTive case
      call SWINE      (TDUSTN)
C     !END
      call BYE ('SINEW')
C
      return
      end
