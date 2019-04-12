      subroutine TAZ
     $(N,IMAX,TAU,FIN,TMS,WH,D,E,S,R)
C
C     Rudolf Loeser, 1989 Jun 28
C---- Actual PHI operator calculation for PUR.
C     (See also SHILKA.)
C     !DASH
      save
C     !DASH
      real*8 D, DT, E, R, S, TAU, TMS, WH
      integer I, IMAX, N
      logical FIN, PAR
C     !DASH
      external ZERO1, ANADYR, SAMOYED, CHITA, BURYAT, HI, BYE
C
C               TAU(N,N), WH(N,N), D(N), E(N), S(N), R(N)
      dimension TAU(N,*), WH(N,*), D(*), E(*), S(*), R(*)
C
      call HI ('TAZ')
C     !BEG
C---- Initialize
      call ZERO1      (WH,(N**2))
C
C---- Compute rows of matrix.
      do 100 I = 1,IMAX
C----   Compute delta-Tau.
        call ANADYR   (TAU,I,N,D)
C----   Compute exponentials, and auxiliary functions.
        call SAMOYED  (N,D,E,S,R)
C----   For finite slab.
        PAR = (TAU(I,I).ge.TMS).and.(I.ne.1).and.(I.ne.N)
        call CHITA    (I,N,WH,PAR,D,E,S,R)
C
        if(.not.FIN) then
C----     Modify for semi-infinite medium.
          DT = TAU(I,N)-TAU(I,I)
          call BURYAT (I,N,WH,D,DT)
        end if
C
  100 continue
C     !END
      call BYE ('TAZ')
C
      return
      end
