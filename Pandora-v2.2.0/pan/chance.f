      subroutine CHANCE
     $(ITAU,I,J,KIJ,FCE,E)
C
C     Rudolf Loeser, 2004 Mar 31
C---- Picks out a value of FCE.
C     !DASH
      save
C     !DASH
      real*8 E, FCE, ONE
      integer I, ITAU, IUL, J, KIJ, N, NL
      logical OK
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
C     !DASH
      external INDXNT, HI, BYE
C
C               KIJ(NL,NL), FCE(N,NT)
      dimension KIJ(NL,*),  FCE(N,*)
C
      call HI ('CHANCE')
C     !BEG
      E = ONE
C
      if(KIJ(I,J).eq.1) then
        call INDXNT (I, J, OK, IUL)
        E = FCE(ITAU,IUL)
      end if
C     !END
      call BYE ('CHANCE')
C
      return
      end
