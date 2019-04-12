      subroutine ESPINA
     $(N,NT,INDEX,PF,PFT)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Computes Partition Function ratios, for CORAL.
C     !DASH
      save
C     !DASH
      real*8 PF, PFT, TWO
      integer I, INDEX, J, J1, J2, KIND, LOOK, LUEO, N, NOTE, NT
      logical OKAY
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
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external LOOKSC, MESHED, ABORT, ARRDIV, CONMUL, LINER, HI, BYE
C
C               INDEX(NMT,2), PF(N,NT), PFT(N,NMT)
      dimension INDEX(NMT,*), PF(N,*),  PFT(N,*)
C
      call HI ('ESPINA')
C     !BEG
      do 103 J = 1,NMT
C
        call LOOKSC   (ELSUB,NMT,ELSYM(J),KIND,NOTE,LOOK)
        if(LOOK.eq.2) then
          KIND = NMT
        end if
C
        OKAY = ((LOOK.eq.1).and.(NOTE.eq.1)).or.(LOOK.eq.2)
        if(.not.OKAY) then
          call MESHED ('ESPINA',1)
          write (LUEO,100)
  100     format(' ','ELSUB')
          write (LUEO,101) (ELSUB(I),I=1,NMT)
  101     format(' ',7X,24(A3,2X))
          call LINER  (2,LUEO)
          write (LUEO,102) LOOK,NOTE,ELSYM(J)
  102     format('LOOK =',I12,', NOTE =',I12,'; cannot find ',A,
     $           ' in ELSUB.')
          call ABORT
        end if
C
        J1 = INDEX(KIND,1)
        J2 = INDEX(KIND,2)
        call ARRDIV (PF(1,J2),PF(1,J1),PFT(1,J),N)
  103 continue
      call CONMUL   (TWO,PFT,(N*NMT))
C     !END
      call BYE ('ESPINA')
C
      return
      end
