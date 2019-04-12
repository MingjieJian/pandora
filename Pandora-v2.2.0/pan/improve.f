      subroutine IMPROVE
     $(FNC,Z,N,CALLER,J,EDFNC)
C
C     Rudolf Loeser, 2005 Mar 11
C---- Checks and edits the values of FNC(Z), length N.
C
C     Some computed tables (such as TAU), to be acceptable, must
C     be increasing funtions of a distance grid (such as Z)---
C     in other words, they must be nondecreasing.
C
C     This routine checks FNC to see whether it is acceptable.
C
C     If acceptable, sets EDFNC = .false. and returns.
C
C     If not acceptable, then IMPROVE attempts to make it acceptable
C     (i.e. to force monotonicity) by inter(extra)polation based on
C     "good" portions  of FNC, and sets EDFNC = .true,
C
C---- Upon return, J .eq. 0 means that FNC is (now) nondecreasing;
C              but J .gt. 0 means that FNC(J) still fails the test.
C     !DASH
      save
C     !DASH
      real*8 FNC, Z
      integer I, J, KODE, LUEO, N
      logical EDFNC, GOOD
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external IS_INCREASING, DODDER, MESHED, MASHED, HI, BYE
C
C               FNC(N), Z(N)
      dimension FNC(*), Z(*)
C
      call HI ('IMPROVE')
C     !BEG
      EDFNC = .false.
C
      call IS_INCREASING     (FNC, 1, N, 0, J)
      GOOD  = J.eq.0
C
      if(.not.GOOD) then
C
        EDFNC = .true.
        do 100 I = 1,N
          call DODDER        (Z, FNC, N, J, KODE)
          if(KODE.eq.0) then
            goto 101
          end if
          call IS_INCREASING (FNC, 1, N, 0, J)
          GOOD = J.eq.0
          if(.not.GOOD) then
            goto 101
          end if
  100   continue
  101   continue
C
        if(.not.GOOD) then
          call MESHED        ('IMPROVE', 3)
          write (LUEO,102) CALLER
  102     format(' ','Called from ',A//
     $           ' ','Unable to force nondecreasing values.')
          call MASHED        ('IMPROVE')
        end if
C
      end if
C     !END
      call BYE ('IMPROVE')
C
      return
      end
