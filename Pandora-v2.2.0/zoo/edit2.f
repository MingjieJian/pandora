      subroutine EDIT2
     $(F,N,CRIT,KODE, R, IMG,BAD)
C
C     Rudolf Loeser, 1990 Jun 28
C
C---- "Editing" utility.
C
C---- EDIT2 is based on KIESEL, first developed in 1990.
C
C---- This is a simple editing procedure.
C
C     All "bad" values of F(i), 1 .le. i .le. N, are identified,
C     and then replaced by the corresponding values of R(i).
C
C---- Input:
C     F     - table of values to be examined and edited
C     R     - table of replacement values
C     N     - length of F and R
C     CRIT  - "bad" value specifier (see KODE)
C     KODE  - "bad" value specifier
C             if KODE = 1, then "bad" means  .LT. CRIT
C                KODE = 2                    .LE. CRIT
C                KODE = 3                    .EQ. CRIT
C                KODE = 4                    .GE. CRIT
C                KODE = 5                    .GT. CRIT
C
C---- Output:
C     F     - some values may have been edited
C     BAD   - is true if there were one or more "bad" values among the
C             input values of F
C
C---- EDIT2 uses some subroutines from the EDIT1-package.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, R
      integer IMG, KODE, N, jummy
      logical BAD
C     !DASH
      external EDICHEK, EDISET2
C
      dimension F(N), R(N), IMG(N)
C
C     !BEG
      BAD = .false.
      if(N.gt.0) then
C----   Check for "bad" values
        call EDICHEK   (F,N,CRIT,KODE,jummy,IMG,BAD)
        if(BAD) then
C----     Fix the "bad" values
          call EDISET2 (F,N,CRIT,KODE,R)
        end if
      end if
C     !END
C
      return
      end
