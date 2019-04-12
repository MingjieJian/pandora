      subroutine LETUP
     $(F,FL,N,NMTS,SIG)
C
C     Rudolf Loeser, 1982 May 14
C---- Computes Logs and edits them, for METALIC.
C     (This is version 2 of LETUP.)
C     !DASH
      save
C     !DASH
      real*8 F, FL, PHI, PLO, SIG, ZHI, ZLO
      integer I, J, N, NMTS
C     !COM
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !DASH
C     !EJECT
      external LOGO, SET1, HI, BYE
C
C               FL(N,NMTS), F(N,NMTS)
      dimension FL(N,*),    F(N,*)
C
      data PHI, ZHI, PLO, ZLO /0.D0, .3D0, -2.D0, -3.D0/
C
      call HI ('LETUP')
C     !BEG
C---- Compute Logs
      call LOGO     (F, (N*NMTS), 1, SIG, FL)
C
C---- Delete column for Hydrogen
      do 100 J = 1,NMTS
        if(ELSUB(J).eq.'H  ') then
          call SET1 (FL(1,J), N, SIG)
        end if
  100 continue
C
C---- Jiggle out-of-range values
      do 102 J = 1,NMTS
        do 101 I = 1,N
C
          if(FL(I,J).ge.PHI) then
            FL(I,J) = ZHI
          else if(FL(I,J).lt.PLO) then
            FL(I,J) = ZLO
          end if
C
  101   continue
  102 continue
C     !END
      call BYE ('LETUP')
C
      return
      end
