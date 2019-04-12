      subroutine ARGUN
     $(N,IMAX,TAU,MOVING,T)
C
C     Rudolf Loeser, 1989 Jun 15
C---- Sets up optical depth array, T, for "GR" matrix calculations.
C
C---- The output array T (size N,N) will have its first IMAX rows
C     filled with optical depth values.
C
C---- If the input value of MOVING = .false., then the input parameter
C     TAU is a vector of size N, and each of the rows of T will be
C     set equal to TAU.
C---- If the input value of MOVING = .true., then the input parameter
C     TAU is an array of the form (N,N) whose rows are generally all
C     different, and each of the rows of T will be set equal to the
C     corresponding rows of TAU.
C     (This is version 3 of ARGUN.)
C     !DASH
      save
C     !DASH
      real*8 T, TAU
      integer I, IMAX, N
      logical MOVING
C     !DASH
      external MOVED, HI, BYE
C
C               MDIM = 1 or N, depending on MOVING
C
C               TAU(N,MDIM), T(N,N)
      dimension TAU(N,*),    T(N,*)
C
      call HI ('ARGUN')
C     !BEG
      do 100 I = 1,IMAX
        if(MOVING) then
          call MOVED (TAU(I,1),N,N,T(I,1),N,N)
        else
          call MOVED (TAU(1,1),1,N,T(I,1),N,N)
        end if
  100 continue
C     !END
      call BYE ('ARGUN')
C
      return
      end
