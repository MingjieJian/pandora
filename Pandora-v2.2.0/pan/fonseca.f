      subroutine FONSECA
     $(INDEX,PF,REF,VAL,SIG,N,NT,NO,IS,IE)
C
C     Rudolf Loeser, 1982 Jun 30
C---- Prints, for GRANADA.
C     !DASH
      save
C     !DASH
      real*8 PF, REF, VAL
      integer I, IE, II, INDEX, ION, IOP, IS, J, JO, K, KNT, L, N, NO,
     $        NT
      character ELE*2, SIG*1
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
      external ESSEX, RINEW, SHIM, HI, BYE
C
C               INDEX(NMT,2), PF(N,NT), REF(N,NT), VAL(10), SIG(10)
      dimension INDEX(NMT,*), PF(N,*),  REF(N,*),  VAL(*),  SIG(*)
C     !EJECT
C
      call HI ('FONSECA')
C     !BEG
      L = 0
      J = 0
      do 101 K = 1,NMT
        call ESSEX   (ELSUB(K),ELE,ION)
        JO = J
        I = INDEX(K,1)
        J = INDEX(K,2)
        if(I.ne.JO) then
          call RINEW (PF(1,I),REF(1,I),IS,IE,VAL,SIG,KNT)
          write (NO,100) ELE,ION,(VAL(II),SIG(II),II=1,KNT)
  100     format(' ',A2,I2,7X,1P,10(E10.3,A1))
          L = L+1
          call SHIM  (L,5,NO)
        end if
        IOP = ION+1
        call RINEW   (PF(1,J),REF(1,J),IS,IE,VAL,SIG,KNT)
        write (NO,100) ELE,IOP,(VAL(II),SIG(II),II=1,KNT)
        L = L+1
        call SHIM    (L,5,NO)
  101 continue
C     !END
      call BYE ('FONSECA')
C
      return
      end
