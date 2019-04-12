      subroutine ADAM
C
C     Rudolf Loeser, 1981 May 26
C---- Checks how input counter values are related to their prescribed
C     limits.
C     (This is version 2 of ADAM.)
C     !DASH
      save
C     !DASH
      integer KWC, LUEO, NAB, NL, NOION, NSL
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(46),KWC)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C     !EJECT
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
      external EVE, AVA, ABORT, HI, BYE
C
      call HI ('ADAM')
C     !BEG
      KILROY = .true.
C
      if(NOION.le.0) then
        call AVA  (                    KILROY, 'ADAM')
C
        call EVE  ('NL ', NL,  NSL,    KILROY, 'ADAM')
        call EVE  ('NSL', NSL, LIMIT,  KILROY, 'ADAM')
      end if
C
      call EVE    ('NMT', NMT, NMTMAX, KILROY, 'ADAM')
      call EVE    ('NAB', NAB, KMXBND, KILROY, 'ADAM')
      call EVE    ('KWC', KWC, KMXWAV, KILROY, 'ADAM')
C
      if(.not.KILROY) then
C       Error stop
        write (LUEO,100)
  100   format(' ','Limits on counters not observed -- check input.')
        call ABORT
      end if
C     !END
      call BYE ('ADAM')
C
      return
      end
