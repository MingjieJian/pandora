      subroutine OSIN
     $(N,NL,XNK,XND,NPQ,LCX,CXXP,CXX,XRK,XRL,LU)
C
C     Rudolf Loeser, 1990 Nov 28
C---- Computes charge exchange terms, for OGBO.
C     !DASH
      save
C     !DASH
      real*8 CXX, CXXP, XND, XNK, XRK, XRL
      integer I, J, LCX, LU, N, NL, NN, NPQ, PQN
      logical PRNTZ
C     !COM
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
C     !DASH
      external ZERO1, ABJECT, LINER, OMAR, HI, BYE
C
C               XNK(N), XND(N,NL), CXXP(N,NL), CXX(N,NL), XRL(N,NPQLM),
      dimension XNK(*), XND(N,*),  CXXP(N,*),  CXX(N,*),  XRL(N,*),
C
C               NPQ(NL), LCX(NL), XRK(N,NPQLM)
     $          NPQ(*),  LCX(*),  XRK(N,*)
C
      data PRNTZ /.false./
C     !EJECT
C
      call HI ('OSIN')
C     !BEG
      NN = N*NPQLM
      call ZERO1    (XRK,NN)
      call ZERO1    (XRL,NN)
C
      do 101 J = 1,NL
        if(LCX(J).gt.0) then
          PQN = NPQ(J)
          if((PQN.ge.4).and.(PQN.le.NPQLM)) then
            do 100 I = 1,N
              XRK(I,PQN) = XRK(I,PQN)+XNK(I  )*CXXP(I,J)
              XRL(I,PQN) = XRL(I,PQN)+XND(I,J)*CXX (I,J)
  100       continue
          end if
        end if
  101 continue
C
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,102)
  102   format(' ','Upper-Level Charge Exchange terms, to be used ',
     $             'in a Hydrogen run.  (Printing controlled by ',
     $             'option CHXPRNT)')
C
        call LINER  (2,LU)
        write (LU,103) 'XRKH'
  103   format(' ',A,5X,'(Level number = the principal quantum ',
     $             'number n for levels of the Hydrogen ion model)')
        call OMAR   (LU,N,NPQMX,XRK,'Level ',PRNTZ)
C
        call LINER  (2,LU)
        write (LU,103) 'XRLH'
        call OMAR   (LU,N,NPQMX,XRL,'Level ',PRNTZ)
      end if
C     !END
      call BYE ('OSIN')
C
      return
      end
