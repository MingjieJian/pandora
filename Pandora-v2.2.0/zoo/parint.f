      subroutine PARINT
     $(XT,INCX,FT,INCF,NT,X,F,KODE,IRET,JS,R)
C
C     Rudolf Loeser, 1981 Feb 24
C     (Originally written for CDC 6400, 1977 Oct 10)
C
C---- Parabolic Inter(Extra)polation Routine.
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
C                 other cases, either straight line interpolation is
C                 used, or parabolic interpolation using the "better"
C                 of the two possible parabolas.
C             =2, then NT .le. 1 and F=FT(1).
C             =3, then X .lt. XT(1), and if
C                 KODE=1, then F=FT(1), but if
C                 KODE=2, then F was obtained by extrapolation.
C             =4, then X .gt. XT(NT), and if
C                 KODE=1, then F=FT(NT), but if
C                 KODE=2, then F was obtained by extrapolation.
C
C---- JS and R (length 4) are storage for intermediate results, whose
C     contents are set up by PARINT for use in a subsequent call,
C     if possible. A particular pair (JS,R) pertains to a particular
C     function table (XT,FT,NT). The first time, only, that PARINT is
C     called with a particular (XT,FT,NT), JS should be initialized to
C     0 by the caller. Thereafter, whenever PARINT is called with a
C     particular (XT,FT,NT), it should also be given the corresponding
C     (JS,R). (This can result in computational savings whenever
C     successive values of X fall into the same interval of (XT,FT,NT).)
C     !DASH
      save
C     !DASH
      real*8 F, FT, R, X, XT
      integer IB, IC, INCF, INCX, IRET, J, JB, JC, JS, KODE, L, NOTE,
     $        NT, NTF, NTX
      logical GOOD
C     !DASH
C     !EJECT
      external SEARCH, LINT, PINT, ABORT
C
      dimension XT(*), FT(*), R(4)
C
C     !BEG
      if(NT.gt.1) then
        NTX = 1+INCX*(NT-1)
        NTF = 1+INCF*(NT-1)
        call SEARCH     (XT, INCX, NT, X, J, NOTE, L)
        IB = 1+INCX*(J-1)
        IC = IB+INCX
        JB = 1+INCF*(J-1)
        JC = JB+INCF
        if(L.eq.1) then
          IRET = 1
          if(NOTE.eq.1) then
            F = FT(JB)
          else if(NOTE.eq.2) then
            call PINT   (XT, INCX, FT, INCF, NT, J, X, F, GOOD, JS, R)
            if(.not.GOOD) then
              call LINT (XT(IB), FT(JB), XT(IC), FT(JC), 1, X, F)
            end if
          else
            write (*,100) NOTE
  100       format(' ','PARINT:  NOTE =',I12,', which is not 1 or 2.')
            call ABORT
          end if
C     !EJECT
        else if(L.eq.2) then
          IRET = 1
          F    = FT(NTF)
        else if(L.eq.3) then
          IRET = 4
          if(KODE.eq.1) then
            F = FT(NTF)
          else if(KODE.eq.2) then
            call LINT   (XT(NTX-INCX), FT(NTF-INCF), XT(NTX), FT(NTF),
     $                   1, X, F)
          else
            write (*,101) KODE
  101       format(' ','PARINT:  KODE =',I12,', which is not 1 or 2.')
            call ABORT
          end if
        else if(L.eq.4) then
          IRET = 3
          if(KODE.eq.1) then
            F = FT(1)
          else if(KODE.eq.2) then
            call LINT   (XT(1), FT(1), XT(1+INCX), FT(1+INCF), 1, X, F)
          else
            write (*,101) KODE
            call ABORT
          end if
        else
          write (*,102) L
  102     format(' ','PARINT:  L =',I12,', which is not 1, 2, 3, or 4.')
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
