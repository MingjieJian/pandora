      subroutine SHILKA
     $(N,IMAX,TAU,FIN,TMS,WN,D,E,S,R,P)
C
C     Rudolf Loeser, 1985 Jan 03
C---- Actual "GR" weight matrix calculation for AMUR.
C     (See also TAZ.)
C     (This is version 2 of SHILKA.)
C     !DASH
      save
C     !DASH
      real*8 D, DT, E, P, R, S, TAU, TMS, WN
      integer I, IMAX, N
      logical FIN, PAR
C     !DASH
      external ZERO1, ANADYR, SAMOYED, LAPTEV, YAKUT, BURYAT, HI, BYE
C
C               TAU(N,N), WN(N,N), D(N), E(N), S(N), R(N), P(N)
      dimension TAU(N,*), WN(*),   D(*), E(*), S(*), R(*), P(*)
C
      call HI ('SHILKA')
C     !BEG
C---- Initialize
      call ZERO1      (WN,(N**2))
C
C---- Compute rows of matrix.
      do 100 I = 1,IMAX
C----   Compute delta-Tau.
        call ANADYR   (TAU,I,N,D)
C----   Compute exponentials and auxiliary functions.
        call SAMOYED  (N,D,E,S,R)
        call LAPTEV   (N,D,E,P)
C----   Compute basic matrix, for finite slab.
        PAR = (TAU(I,I).ge.TMS).and.(I.ne.1).and.(I.ne.N)
        call YAKUT    (I,N,WN,PAR,D,E,S,R,P)
C
        if(.not.FIN) then
C----     Modify for semi-infinite medium.
          DT = TAU(I,N)-TAU(I,I)
          call BURYAT (I,N,WN,D,DT)
        end if
C
  100 continue
C     !END
      call BYE ('SHILKA')
C
      return
      end
