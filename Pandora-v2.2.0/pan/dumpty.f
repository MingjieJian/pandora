      subroutine DUMPTY
     $(INDX,XLM,N,NOPAC,HND,XLMDUST,DFDUST,ALBDUST,XLDT,XFRQ,ADT,ALBDT,
     $ KILROY,COEFF,ALBEDO,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of Dust Absorption values.
C     (This is version 2 of DUMPTY.)
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, ALBDUST, ALBEDO, COEFF, CONT, DFDUST, FAC, HND,
     $       ONE, XFRQ, XLDT, XLM, XLMDUST
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DUST, HI, BYE
C
C               XLMDUST(NDT), DFDUST(NDT), ALBDUST(NDT), CONT(Nopac,N),
      dimension XLMDUST(*),   DFDUST(*),   ALBDUST(*),   CONT(NOPAC,*),
C
C               XLDT(NDT), ALBDT(NDT), ADT(NDT), HND(N), XFRQ(NDT)
     $          XLDT(*),   ALBDT(*),   ADT(*),   HND(*), XFRQ(*)
C
      call HI ('DUMPTY')
C     !BEG
      call DUST (LDU, XLMDUST, DFDUST, ALBDUST, NDT, XLDT, ADT, ALBDT,
     $           XFRQ, KILROY, XLM, COEFF, ALBEDO)
      FAC = COEFF*(ONE-ALBEDO)
C
      do 100 J = 1,N
        CONT(INDX,J) = FAC*HND(J)
  100 continue
C     !END
      call BYE ('DUMPTY')
C
      return
      end
