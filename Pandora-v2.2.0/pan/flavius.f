      subroutine FLAVIUS
     $(N,NSHL,NRPMX,TSHL,CODSRW,Y,MOVING,WNZ,WNR,WHZ,WHR,ILFLX,TAU,
     $ WRK,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 15
C---- Computes Shell weight matrices directly, for FERREX.
C     (This is version 3 of FLAVIUS.)
C     !DASH
      save
C     !DASH
      real*8 CODSRW, TAU, TSHL, W, WHR, WHZ, WNR, WNZ, WRK, Y, ZERO,
     $       dummy
      integer I, ILFLX, IMX, IW, J, N, NRP, NRPMX, NSHL, jummy
      logical DUMP, FIN, GDIL, MOVING, TAURED
      character LAB*14, qummy*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external TATAR, LAMBDA, COYOTE, NIDABA, ARGUN, PHI, HI, BYE
C
      dimension W(*), IW(*)
C
C               TSHL(NRPMX,NSHL), TAU(NRPMX,NRPMX), WNR(NRPMX,NRPMX),
      dimension TSHL(NRPMX,*),    TAU(*),           WNR(*),
C
C               WNZ(N,N,NSHL), WRK(NRPMX,NRPMX), WHR(NRPMX,NRPMX),
     $          WNZ(N,N,*),    WRK(*),           WHR(*),
C
C               CODSRW(NSHL), WHZ(N,N,NSHL)
     $          CODSRW(*),    WHZ(N,N,*)
C
      data FIN    /.true./
      data TAURED /.true./
      data GDIL   /.false./
C     !EJECT
C
      call HI ('FLAVIUS')
C     !BEG
      I = 0
C
C---- Loop over all rays
      do 101 J = 1,NSHL
C----   Get I, the ray index
        call TATAR      (I)
C
        if(CODSRW(J).eq.ZERO) then
          NRP = 2*I+5
          IMX = I+3
C
C----     Set up TAUs for this ray
          call ARGUN    (NRP, IMX, TSHL(1,J), MOVING, TAU)
C----     Compute WNR
          call LAMBDA   (dummy, W, IW, TAU, IMX, NRP, Y, FIN, TAURED,
     $                   GDIL, jummy, qummy, WNR)
C----     Transform WNR to WNZ
          call NIDABA   (I, WNR, WNZ(1,1,J), W)
C
          if(ILFLX.gt.0) then
C----       Compute WHR
C           (computed here because it too needs TAU)
            call PHI    (TAU, IMX, NRP, Y, FIN, WHR, W)
C----       Transform WHR to WHZ
            call NIDABA (I, WHR, WHZ(1,1,J), W)
          end if
C
          if(DUMP) then
            write (LAB,100) J,I
  100       format('Shell ',I3,'(',I3,')')
            call COYOTE (LAB, I, NRP, TSHL(1,J), WNR, WHR, WRK, ILFLX)
          end if
        end if
C
  101 continue
C     !END
      call BYE ('FLAVIUS')
C
      return
      end
