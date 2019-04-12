      subroutine WARNINC
     $(A,N,TIT,CALLER,BAD)
C
C     Rudolf Loeser, 2005 Mar 14
C---- Checks array A (length N) for strict monotonic increase.
C
C     R e s e t s   BAD = .true. and prints a warning
C     if A fails the test;
C
C     does nothing if A passes.
C     !DASH
      save
C     !DASH
      real*8 A
      integer J, LUEO, N
      logical AZERO, BAD, GOOD
      character CALLER*(*), TIT*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, IS_INCREASING, MESHED, MASHED, DVECOUT, HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('WARNINC')
C     !BEG
      call NAUGHTD         (A, 1, N, AZERO)
C
      if(.not.AZERO) then
        call IS_INCREASING (A, 1, N, 1, J)
        GOOD = J.eq.0
C
        if(.not.GOOD) then
          call MESHED      ('WARNINC', 3)
          call DVECOUT     (LUEO, A, N, TIT)
          write (LUEO,100) J,CALLER
  100     format(' ','The table is not strictly monotonic at the',I8,
     $               'th point.'/
     $           ' ','Called from: ',A)
          call MASHED      ('WARNINC')
C
          BAD = .true.
        end if
C
      end if
C     !END
      call BYE ('WARNINC')
C
      return
      end
