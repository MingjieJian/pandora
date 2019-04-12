      subroutine PARINTR
     $(XT,INCX,FT,INCF,NT,X,FX,KODE,IRET,JS,R)
C     Rudolf Loeser, 1992 Aug 07
C     (This is version 2 of PARINTX)
C     (Originally written for CDC 6400, 1977 Oct 10)
C
C     Parabolic Inter(Extra)polation Routine;
C     for real*4 operands.
C
C---- Given the table XT of length NT, NT .gt. 1, and the
C     corresponding table FT, values of a function of XT. The values
C     of XT must all be distinct, and must be arranged in ascending
C     algebraic order, i.e. XT(j+1) .gt. XT(j), 1 .le. j .lt. NT.
C
C---- Successive elements of "XT" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "XT" lives in XT(II), where II=1+INCX*(I-1).
C
C---- Successive elements of "FT" are stored in memory locations
C     separated by the constant stride INCF, INCF > 0, such that the
C     I'th element of "FT" lives in FT(II), where II=1+INCF*(I-1).
C
C---- Given the switch KODE, such that
C     KODE=1 means - do not extrapolate; and
C     KODE=2 means - extrapolate when X lies outside the range of XT.
C
C---- Upon return,
C     if  IRET=1, then FX is the value of the function evaluated
C                 as follows: straight-line interpolation is used
C                 whenever X .lt. XT(2) or X .gt. XT(NT-1); in all
C                 other cases, either straight line interpolation is
C                 used, or parabolic interpolation using the "better"
C                 of the two possible parabolas.
C             =2, then NT .le. 1 and FX=FT(1).
C             =3, then X .lt. XT(1), and if
C                 KODE=1, then FX=FT(1), but if
C                 KODE=2, then FX was obtained by extrapolation.
C             =4, then X .gt. XT(NT), and if
C                 KODE=1, then FX=FT(NT), but if
C                 KODE=2, then FX was obtained by extrapolation.
C
C---- JS (length 1) and R (length 4) are storage for intermediate
C     results, whose contents are set up by PARINTX for use in a
C     subsequent call, if possible. A particular pair (JS,R) pertains
C     to a particular function table (XT,FT,NT). The first time,
C     only, that PARINTX is called with a particular (XT,FT,NT), JS
C     should be initialized to 0 by the caller. Thereafter, whenever
C     PARINTX is called with a particular (XT,FT,NT), it should also be
C     given the corresponding (JS,R). (This can result in computational
C     savings whenever successive values of X fall into the same
C     interval of the table (XT,FT,NT).)
C     !DASH
      save
C     !DASH
      real*4 FT, FX, R, X, XT
      integer INCF, INCX, IRET, J, JF, JS, KODE, L, NOTE, NT, NTF, NTX
C     !DASH
C     !EJECT
      external SEARCHR, LINTR, PINTR, ABORT
C
      dimension XT(*), FT(*), R(4)
C
C     !BEG
      if(NT.gt.1) then
        NTX = 1+INCX*(NT-1)
        NTF = 1+INCF*(NT-1)
        call SEARCHR   (XT,INCX,NT,X,J,NOTE,L)
        if(L.eq.1) then
          IRET = 1
          if(NOTE.eq.1) then
            JF = 1+INCF*(J-1)
            FX = FT(JF)
          else if(NOTE.eq.2) then
            call PINTR (XT,INCX,FT,INCF,NT,J,X,FX,JS,R)
          else
            write (*,100) 'NOTE', NOTE
  100       format(' ','Error in PARINTR: ',A,' =',I12)
            call ABORT
          end if
        else if(L.eq.2) then
          IRET = 1
          FX   = FT(NTF)
        else if(L.eq.3) then
          IRET = 4
          if(KODE.eq.1) then
            FX = FT(NTF)
          else if(KODE.eq.2) then
            call LINTR (XT(NTX-INCX),FT(NTF-INCF),XT(NTX),FT(NTF),
     $                  1,X,FX)
          else
            write (*,100) 'KODE', KODE
            call ABORT
          end if
        else if(L.eq.4) then
          IRET = 3
          if(KODE.eq.1) then
            FX = FT(1)
          else if(KODE.eq.2) then
            call LINTR (XT(1),FT(1),XT(1+INCX),FT(1+INCF),1,X,FX)
          else
            write (*,100) 'KODE', KODE
            call ABORT
          end if
        else
          write (*,100) 'L', L
          call ABORT
        end if
      else
        IRET = 2
        FX   = FT(1)
      end if
C     !END
C
      return
      end
