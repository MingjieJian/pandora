      subroutine COCK
     $(MODE,LODE,FVAL,IVAL,FLOATS,INTS,N,NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Processes data fields for ARRAN.
C     !DASH
      save
C     !DASH
      real*8 FLOATS, FVAL, dummy
      integer IL, INTS, IVAL, KERR, LODE, LUEO, MODE, N, jummy
      logical FLPT
      character ALF*8, NAME*8
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C     !DASH
      external  KIWI, CHLOE, MESHED, ABORT, CARMEN, HI, BYE
      intrinsic min
C
C               FLOATS(N), INTS(N)
      dimension FLOATS(*), INTS(*)
C     !EJECT
C
      call HI ('COCK')
C     !BEG
      KERR = 0
      FLPT = LODE.eq.1
      if(MODE.eq.2) then
C
C             M    F
        goto (100, 101 ), KIND
C
  100   continue
C----     Process multiplier specification
          if(FLPT) then
            call KIWI (MODE, FM, jummy, ALF, jummy)
            if(MODE.ne.5) goto 204
            goto 199
          else
            call KIWI (MODE, dummy, IM, ALF, jummy)
            if(MODE.ne.3) goto 203
            goto 199
          end if
  101   continue
C----     Process fill specification
          K = N
          goto 199
      else if(MODE.eq.3) then
C----   Process integer array element
        if(FLPT) goto 204
        IVAL = IM*IVAL
        goto 102
      else if(MODE.eq.5) then
C----   Process floating point array element
        if(.not.FLPT) goto 203
        FVAL = FM*FVAL
        goto 102
      else
        goto 209
      end if
  102 continue
C----   Store current value in array, K times
        if(IN.gt.N) goto 210
        IL = min((IN+(K-1)),N)
        do 103 K = IN,IL
          if(FLPT) then
            FLOATS(K) = FVAL
          else
            INTS  (K) = IVAL
          end if
  103   continue
        IN = IL+1
        K  = 1
        goto 199
C     !EJECT
C---- Error processing
  210 KERR = KERR+1
  209 KERR = KERR+1
  208 KERR = KERR+1
  207 KERR = KERR+1
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('COCK', 1)
      write (LUEO,200) NAME,N
  200 format(' ','Error reading for: ',A8,'  of length',I6)
      call CHLOE  (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('COCK')
C
      return
      end
