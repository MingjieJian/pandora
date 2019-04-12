      subroutine LALA
     $(IU,IL,AIJ,YBRIJ,X,CUE)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes the "single-rate Q" for the VAMOS method.
C     !DASH
      save
C     !DASH
      real*8 AIJ, CUE, ONE, X, YARAT, YBRIJ
      integer IL, IU, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIATA, HI, BYE
C
      dimension X(*)
C
C               AIJ(NL,NL), YBRIJ(NL,NL)
      dimension AIJ(NL,*),  YBRIJ(*)
C
      call HI ('LALA')
C     !BEG
      call RIATA (X, IU, IL, NL, YBRIJ, YARAT)
      CUE = AIJ(IU,IL)*(ONE+YARAT)
C     !END
      call BYE ('LALA')
C
      return
      end
