      subroutine ALBION
     $(NO,MSFQR,MSFQM,MSFRT,MSFGR,LCOW)
C
C     Rudolf Loeser, 1990 Dec 11
C---- Prints numerological parameters.
C     (This is version 3 of ALBION.)
C     !DASH
      save
C     !DASH
      integer LCOW, MSFGR, MSFQM, MSFQR, MSFRT, NO
C     !COM
C---- MATRIX      as of 2006 Sep 06
      integer     PRNSW,EDJSW,KNTIN,KNTED
      real*8      CRITJ,TIMIN,TIMED
      common      /MATRIX1/ PRNSW,EDJSW,KNTIN,KNTED
      common      /MATRIX2/ CRITJ,TIMIN,TIMED
C
C     Control parameters for matrix inversion and determinants.
C
C     PRNSW = 1: print matrix messages; = 0: do not.
C     EDJSW = 1: edit out "junk" using CRITJ; = 0: do not.
C     KNTIN      count of calls to INVERS.
C     KNTED      count of calls to DETERM.
C     CRITJ      "junk" criterion for EDJSW.
C     TIMIN      total time for all matrix inversions.
C     TIMED      total time for all determinants.
C     .
C     !DASH
C     !EJECT
      external PADMA, LINER, SMILAX, HI, BYE
C
      call HI ('ALBION')
C     !BEG
      if(NO.gt.0) then
        call PADMA  (NO, 'Numerology')
C
        write (NO,100)
  100   format(' ','For matrix inversion and determinant calculation ',
     $             'PANDORA uses L\U decomposition by Crout''s ',
     $             'algorithm,'/
     $         ' ','as worked out in "Numerical Algorithms" by ',
     $             'Press et al.')
C
        call LINER  (1, NO)
        write (NO,101) PRNSW,EDJSW
  101   format(' ',I9,' DRPSW: switch for printing of messages from ',
     $                'each matrix inversion and determinant ',
     $                'calculation'/
     $         ' ',I9,' EDJSW: switch for "junk editing" of matrices ',
     $                'for inversions and determinant calculations')
        if(EDJSW.ne.0) then
          write (NO,102) CRITJ
  102     format(' ',1PE9.2,' CRITJ: criterion for EDJSW')
        end if
C
        call SMILAX (NO, MSFQR, MSFQM, MSFRT, MSFGR, LCOW)
      end if
C     !END
      call BYE ('ALBION')
C
      return
      end
