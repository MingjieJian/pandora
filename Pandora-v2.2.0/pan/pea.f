      subroutine PEA
     $(NO)
C
C     Rudolf Loeser, 1996 Jan 04
C---- Provides alert messages concerning ABD/KOPAC inconsistencies.
C     !DASH
      save
C     !DASH
      real*8 ZERO
      integer I, IABS, IELE, JA, JE, KNT, KO, NO
      logical GOOD, KILROY
      character NOYES*3
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      parameter (KNT=13)
      dimension IABS(KNT), IELE(KNT), NOYES(2)
C
      data KILROY,NOYES /.true., 'no ', 'yes'/
C
      data IABS /  6,  7,  8, 10, 12, 17, 18, 19, 20, 21, 29, 33,  9/
      data IELE / 17, 15, 13,  7, 14, 27, 12, 21,  2,  3,  2,  3,  9/
C
      call HI ('PEA')
C     !BEG
      if(NO.gt.0) then
C
        do 101 I = 1,KNT
          JA = IABS(I)
          KO = KOPAC(JA)
C
          JE = IELE(I)
          if(ELABD(JE).eq.ZERO) then
            GOOD = KO.eq.0
          else
            GOOD = KO.eq.1
          end if
C
          if(.not.GOOD) then
            if(KILROY) then
              call LINER (5, NO)
              KILROY = .false.
            end if
            write (NO,100) ELSYM(JE),ELABD(JE),CNAME(JA),NOYES(KO+1)
  100       format(' ','Note: ',A,': abundance =',1PE12.4,'  <-->  ',
     $                 '"continuum" contributor: ',A,' = ',A)
          end if
  101   continue
C
      end if
C     !END
      call BYE ('PEA')
C
      return
      end
