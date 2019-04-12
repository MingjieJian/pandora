      subroutine CRANKY
     $(ITAU,IU,IL,KIJ,SET,XQ)
C
C     Rudolf Loeser, 2004 Mar 26
C---- Computes XQ(u,l), a number density test criterion.
C     (This is version 2 of CRANKY.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SET, TWO, XQ
      integer IL, ITAU, IU, IUL, KIJ, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external INDXUL, DIVIDE, HI, BYE
C
C               KIJ(NL,NL), SET(N,MUL)
      dimension KIJ(NL,*),  SET(N,*)
C
      call HI ('CRANKY')
C     !BEG
      XQ = TWO
C
      if(KIJ(IU,IL).eq.1) then
C       Compute for radiative transitions only
        call INDXUL (IU, IL, IUL)
        call DIVIDE (ONE, SET(ITAU,IUL), XQ)
      end if
C     !END
      call BYE ('CRANKY')
C
      return
      end
