      subroutine EDIT1
     $(F,N, CRIT,KODE,MODE, IMG,BAD,ALLBAD)
C
C     Rudolf Loeser, 1990 Jun 19
C
C---- "Editing" utility.
C
C---- (EDIT1 is based on EDITH, which was first developed in 1981.)
C
C---- This is a simple editing procedure.
C
C     Sequences (of one or more) "bad" values of F(i), 1 .le. i .le. N,
C     are identified and replaced if possible. If the "bad sequence"
C     includes F(1), then all the values of that sequence are replaced
C     by the first good value; if the "bad sequence" includes F(N),
C     then all the values of that sequence are replaced by the last
C     good value. In the 'normal' case, however, a "bad sequence" has
C     bracketing good values on both sides, and all the values of that
C     sequence are replaced by values obtained by interpolating between
C     the closest bracketing good values. For this interpolation, the
C     index of the F-values serves as the independent variable. This
C     interpolation uses either the given values of F, or their logs.
C
C---- Input:
C     F      - table of values to be examined and edited
C     N      - length of F
C     CRIT   - "bad" value specifier (see KODE)
C     KODE   - "bad" value specifier
C              if KODE = 1, then "bad" means  .LT. CRIT
C                 KODE = 2                    .LE. CRIT
C                 KODE = 3                    .EQ. CRIT
C                 KODE = 4                    .GE. CRIT
C                 KODE = 5                    .GT. CRIT
C     MODE   - interpolation specifier
C              if MODE = 1, then use linear      interpolation
C                 MODE = 2,          logarithmic interpolation
C
C---- Output:
C     F      - some values may have been edited
C     BAD    - is true if there were one or more "bad" values among the
C              input values of F
C     ALLBAD - is true if all input values were bad
C     !DASH
      save
C     !DASH
      real*8 CRIT, F
      integer IMG, K, KODE, MODE, N
      logical ALLBAD, BAD
C     !DASH
      external EDICHEK, EDITSET
C
      dimension F(N), IMG(N)
C     !EJECT
C
C     !BEG
      BAD    = .false.
      ALLBAD = .false.
      if(N.gt.0) then
C----   Check for "bad" values
        call EDICHEK     (F,N,CRIT,KODE,K,IMG,BAD)
        if(BAD) then
          if(K.eq.N) then
C----       Nothing can be done - quit
            ALLBAD = .true.
          else
C----       Fix the bad values
            call EDITSET (F,N,CRIT,KODE,MODE)
          end if
        end if
      end if
C     !END
C
      return
      end
