      subroutine PUNT
     $(LU,XVEC,N,MODE,LABEL)
C
C     Rudolf Loeser, 2002 Feb 05
C
C---- Writes XVEC, length N, in RESTART format, to unit LU.
C
C     MODE = 1 means:  9 figures ("single" precision);
C     MODE = 2 means: 15 figures ("double" precision).
C
C     Drivers:  PANT  for arrays of size "N" x m (one index);
C               BUNT  for vectors of size "N", no indices;
C               PENT  for vectors of size "N", with transition indices.
C     !DASH
      save
C     !DASH
      real*8 XVEC
      integer LU, MODE, N
      logical WRAP, ZVEC
      character BLANK*1, LABEL*(*), MABEL*17
C     !DASH
      external NAUGHTD, PUN, PUD, HI, BYE
C
C               XVEC(N)
      dimension XVEC(*)
C
      data BLANK /' '/
C
      call HI ('PUNT')
C     !BEG
      if(LU.gt.0) then
        call NAUGHTD (XVEC, 1, N, ZVEC)
        if(.not.ZVEC) then
          MABEL = LABEL
          WRAP  = MABEL.ne.BLANK
          if(WRAP) then
            write (LU,100) MABEL
  100       format(A17,' ( > ')
          end if
          if(MODE.eq.2) then
            call PUD (XVEC, 1, N, 1, WRAP, LU)
          else
            call PUN (XVEC, 1, N, 1, WRAP, LU)
          end if
        end if
      end if
C     !END
      call BYE ('PUNT')
C
      return
      end
