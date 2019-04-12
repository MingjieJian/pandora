      subroutine FERIA
     $(N,FMC,AMUX,IMUX,IPNT,IWRK)
C
C     Rudolf Loeser, 1982 Jun 07
C---- Gets sorted list of element indices, for NASSAU.
C     !DASH
      save
C     !DASH
      real*8 AMUX, FMC, ZERO
      integer IMAX, IMIN, IMUX, IPNT, IWRK, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
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
      external MINMAXD, SORT, ORDERI, HI, BYE
C
C               FMC(N,NMT), AMUX(NMT), IMUX(NMT), IPNT(NMT), IWRK(NMT)
      dimension FMC(N,*),   AMUX(*),   IMUX(*),   IPNT(*),   IWRK(*)
C
      call HI ('FERIA')
C     !BEG
      do 100 J = 1,NMT
        IMUX(J) = J
        if(LATEM(J)) then
          call MINMAXD (FMC(1,J), 1, N, IMIN, IMAX)
          AMUX(J) = -FMC(IMAX,J)
        else
          AMUX(J) = ZERO
        end if
  100 continue
C
      if(NMT.gt.1) then
        call SORT      (AMUX, NMT, IPNT, 'Element sort keys')
        call ORDERI    (IMUX, IPNT, NMT, IWRK)
      end if
C     !END
      call BYE ('FERIA')
C
      return
      end
