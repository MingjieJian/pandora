      subroutine HUMPTY
     $(INDX,XLM,N,NOPAC,HND,XLMDUST,DFDUST,ALBDUST,XLDT,XFRQ,ADT,ALBDT,
     $ KILROY,COEFF,ALBEDO,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Dust Scattering values.
C     (This is version 2 of HUMPTY.)
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, ALBDUST, ALBEDO, COEFF, CONT, DFDUST, FAC, HND,
     $       XFRQ, XLDT, XLM, XLMDUST
      integer INDX, J, LDU, N, NDT, NOPAC
      logical KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(26),LDU)
      equivalence (JZQ(21),NDT)
C     !DASH
      external DUST, HI, BYE
C
C               XLMDUST(LDU), DFDUST(LDU), ALBDUST(LDU), CONT(Nopac,N),
      dimension XLMDUST(*),   DFDUST(*),   ALBDUST(*),   CONT(NOPAC,*),
C
C               XLDT(LDT), ALBDT(LDT), ADT(LDT), HND(N), XFRQ(NDT)
     $          XLDT(*),   ALBDT(*),   ADT(*),   HND(*), XFRQ(*)
C
      call HI ('HUMPTY')
C     !BEG
      call DUST (LDU, XLMDUST, DFDUST, ALBDUST, NDT, XLDT, ADT, ALBDT,
     $           XFRQ, KILROY, XLM, COEFF, ALBEDO)
      FAC = COEFF*ALBEDO
C
      do 100 J = 1,N
        CONT(INDX,J) = FAC*HND(J)
  100 continue
C     !END
      call BYE ('HUMPTY')
C
      return
      end
