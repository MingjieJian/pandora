      subroutine FRANK
     $(NAME,NUM,AB,U1,U2,CH,KODE)
C
C     Rudolf Loeser, 1981 May 18
C---- Obtains from the "ELEMENT TABLES"
C     the values of: AB - abundance,
C                 U1,U2 - partition functions, and
C                    CH - ionization potential,
C     for a particular element specified by "NAME" or "NUM", as follows:
C     1)  If "NUM" .eq. 0, then "NAME" must be a valid element symbol.
C         If a match for "NAME" is found, then the corresponding data
C         will be returned, and KODE set equal to the table index of
C         the element; if no match, then zeroes will be returned,
C         and KODE set =0.
C     2)  If 1 .le. "NUM" .le. NMT, then "NUM" identifies an element
C         by its position in the tables. The relevant data are returned,
C         KODE set =NUM, and "NAME" is set equal to the symbol of that
C         element;
C     3)  If "NUM" .lt. 0, or "NUM" .gt. NMT, then abort.
C     !DASH
      save
C     !DASH
      real*8 AB, CH, U1, U2, ZERO
      integer I, K, KODE, NUM
      character NAME*3
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
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HALT, HI, BYE
C
      call HI ('FRANK')
C     !BEG
      if((NUM.lt.0).or.(NUM.gt.NMT)) then
        write (MSSLIN(1),100) NMT,NUM
  100   format('NMT =',I8,'. NMT =',I12,', which is not right.')
        call HALT ('FRANK', 1)
      end if
C
      K = NUM
      if(K.eq.0) then
        do 101 I = 1,NMT
          if(ELSYM(I).eq.NAME) then
            K = I
            goto 102
          end if
  101   continue
  102   continue
      else
        NAME = ELSYM(K)
      end if
C
      if(K.eq.0) then
        AB = ZERO
        CH = ZERO
        U1 = ZERO
        U2 = ZERO
        KODE = 0
      else
        AB = ELABD(K)
        CH = ELCHI(K)
        U1 = ELLU1(K)
        U2 = ELLU2(K)
        KODE = K
      end if
C     !END
      call BYE ('FRANK')
C
      return
      end
