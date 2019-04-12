      subroutine BRAT
     $(I,J,K,BDIJ,RATIO)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Returns the value of B(J)/B(K) at depth I.
C     (This is version 4 of BRAT.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, ONE, RATIO, ZERO
      integer I, J, K, LUEO, N, NL
      logical IBAD, JBAD, KBAD
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MESHED, ABORT, HI, BYE
C
C               BDIJ(N,NL)
      dimension BDIJ(N,*)
C     !EJECT
C
      call HI ('BRAT')
C     !BEG
      IBAD = (I.lt.1).or.(I.gt.N )
      JBAD = (J.lt.1).or.(J.gt.NL)
      KBAD = (K.lt.1).or.(K.gt.NL)
C
      if(IBAD.or.JBAD.or.KBAD) then
        call MESHED   ('BRAT', 1)
        write (LUEO,100) I,J,K,N,NL,NL
  100   format(' ','Trouble computing B ratio: index error.'//
     $             'I =',I10,',   J =',I10,',   K =',I10/
     $             'N =',I10,',  NL =',I10,',  NL =',I10)
        call ABORT
      end if
C
C
C     BDIJ is an array such that:  BDIJ(i,j) = b(i, j) / b(i, 1) .
C
C
C     Given i,J,K   ----   determine RATIO = b(i, J) / b(i, K) .
C
C
      RATIO = ONE
      if(J.ne.K) then
        if(K.eq.1) then
          RATIO = BDIJ(I,J)
C
        else if(J.eq.1) then
          if(BDIJ(I,K).ne.ZERO) then
            RATIO = ONE/BDIJ(I,K)
          end if
        else
C
          if((BDIJ(I,J).ne.ZERO).and.(BDIJ(I,K).ne.ZERO)) then
            RATIO = BDIJ(I,J)/BDIJ(I,K)
          end if
        end if
      end if
C     !END
      call BYE ('BRAT')
C
      return
      end
