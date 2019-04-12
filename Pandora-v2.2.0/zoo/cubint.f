      subroutine CUBINT
     $(XT,INCX,FT,INCF,NT,X,F,KODE,IRET,JS,C)
C
C     Rudolf Loeser, 2006 Dec 08
C---- Cubic Inter(Extra)polation Routine.
C     (CUBINT is based on subroutine PARINT.)
C
C     Given the table XT of length NT, NT .gt. 1, and the
C     corresponding table FT, values of a function of XT. The values
C     of XT must all be distinct, and must be arranged in ascending
C     algebraic order, i.e. XT(j+1) .gt. XT(j), 1 .le. j .lt. NT.
C       Successive elements of "XT" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "XT" lives in XT(II), where II=1+INCX*(I-1).
C       Successive elements of "FT" are stored in memory locations
C     separated by the constant stride INCF, INCF > 0, such that the
C     I'th element of "FT" lives in FT(II), where II=1+INCF*(I-1).
C
C---- Given the switch KODE, such that
C     KODE=1 means - do not extrapolate; and
C     KODE=2 means - extrapolate when X lies outside the range of XT.
C
C---- Upon return,
C     if  IRET=1, then F is the value of the function evaluated
C                 as follows: straight-line interpolation is used
C                 whenever X .lt. XT(2) or X .gt. XT(NT-1); in all
C                 other cases, either cubic interpolation is used
C                 if possible; otherwise, linear interpolation.
C             =2, then NT .le. 1 and F=FT(1).
C             =3, then X .lt. XT(1), and if
C                 KODE=1, then F=FT(1), but if
C                 KODE=2, F was obtained by linear extrapolation.
C             =4, then X .gt. XT(NT), and if
C                 KODE=1, then F=FT(NT), but if
C                 KODE=2, F was obtained by linear extrapolation.
C
C---- JS and C (length 4) are storage for intermediate results, whose
C     contents are set up by CUBINT for use in a subsequent call,
C     if possible. A particular pair (JS,C) pertains to a particular
C     function table (XT,FT,NT). The first time, only, that CUBINT is
C     called with a particular (XT,FT,NT), JS should be initialized to
C     0 by the caller. Thereafter, whenever CUBINT is called with a
C     particular (XT,FT,NT), it should also be given the corresponding
C     (JS,C). (This can result in computational savings whenever
C     successive values of X fall into the same interval of (XT,FT,NT).)
C     !DASH
      save
C     !DASH
      real*8 C, F, F0, FL, FM, FP, FT, R, X, XT
      integer IB, IC, INCF, INCX, IRET, J, JB, JC, JR, JS, KODE, L,
     $        NOTE, NT, NTF, NTX
      logical GOOD
C     !DASH
C     !EJECT
      external SEARCH, LINT, PINT, QINT, TINT, ZINT, ABORT
C
      dimension XT(*), FT(*), C(4), R(4)
C
C     !BEG
      if(NT.gt.1) then
        call SEARCH         (XT, INCX, NT, X, J, NOTE, L)
C----   Set up indices for XT and FT, such that IB and IC pick out
C       successive values of XT (with IB picking out the J'th value);
C       and JB and JC similarly for FT. (This is necessary because the
C       strides INCX and INCF may differ from 1; see above.)
        IB = 1+INCX*(J-1)
        IC = IB+INCX
        JB = 1+INCF*(J-1)
        JC = JB+INCF
C----   Set up indices for the last values of XT and FT
        NTX = 1+INCX*(NT-1)
        NTF = 1+INCF*(NT-1)
        if(L.eq.1) then
          IRET = 1
          if(NOTE.eq.1) then
C----       X equals XT(J)
            F  = FT(JB)
          else if(NOTE.eq.2) then
C----       X lies between XT(J) and XT(J+1): try cubic interpolation
            call QINT       (XT, INCX, FT, INCF, NT, J, X, F, GOOD,
     $                       JS, C)
            if(GOOD) then
C----         Compute test values
              call ZINT     (XT, INCX, FT, INCF, J, X, FM, FP, FL, F0)
C----         Test F for acceptability
              call TINT     (F, FM, FP, FL, F0, GOOD)
              if(.not.GOOD) then
C----           Try parabolic interpolation
                JR = 0
                call PINT   (XT, INCX, FT, INCF, NT, J, X, F, GOOD,
     $                       JR, R)
                if(GOOD) then
C----             Test F for acceptability
                  call TINT (F, FM, FP, FL, F0, GOOD)
                  if(.not.GOOD) then
C----               Replace unacceptable value
                    F = F0
                  end if
                else
C----             Use linear interpolation
                  call LINT (XT(IB), FT(JB), XT(IC), FT(JC), 1, X, F)
                end if
              end if
            else
C----         Use linear interpolation
              call LINT     (XT(IB), FT(JB), XT(IC), FT(JC), 1, X, F)
            end if
          else
            write (*,100) NOTE
  100       format(' ','CUBINT:  NOTE =',I12,', which is not 1 or 2.')
            call ABORT
          end if
C     !EJECT
        else if(L.eq.2) then
C----     X equals X(NT)
          IRET = 1
          F    = FT(NTF)
        else if(L.eq.3) then
C----     X is greater than XT(NT)
          IRET = 4
          if(KODE.eq.1) then
C----       Use final value wherever X is greater than X(NT)
            F = FT(NTF)
          else if(KODE.eq.2) then
C----       Use linear extrapolation
            call LINT   (XT(NTX-INCX), FT(NTF-INCF), XT(NTX), FT(NTF),
     $                   1, X, F)
          else
            write (*,101) KODE
  101       format(' ','CUBINT:  KODE =',I12,', which is not 1 or 2.')
            call ABORT
          end if
        else if(L.eq.4) then
C----     X is less than XT(1)
          IRET = 3
          if(KODE.eq.1) then
C----       Use first value wherever X is less than XT(1)
            F = FT(1)
          else if(KODE.eq.2) then
C----       Use linear extrapolation
            call LINT   (XT(1), FT(1), XT(1+INCX), FT(1+INCF), 1, X, F)
          else
            write (*,101) KODE
            call ABORT
          end if
        else
          write (*,102) L
  102     format(' ','CUBINT:  L =',I12,', which is not 1, 2, 3, or 4.')
          call ABORT
        end if
      else
        IRET = 2
        F    = FT(1)
      end if
C     !END
C
      return
      end
