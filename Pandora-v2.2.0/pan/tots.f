      subroutine TOTS
     $(NODE,FNU,FAL)
C
C     Rudolf Loeser, 1979 Nov 30
C---- Gets Alpha-related factor, for SPUME.
C     !DASH
      save
C     !DASH
      real*8 ALPHA, FAL, FNU, ONE
      integer INK, NODE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(35),INK)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, VALVE, HI, BYE
C
      call HI ('TOTS')
C     !BEG
      FAL = ONE
C
      if(INK.gt.1) then
C
        call VALVE    (FNU,ALPHA)
        if(NODE.eq.2) then
          call DIVIDE (ONE,ALPHA,FAL)
        end if
C
      else
C
        call VALVE    (FNU,ALPHA)
        if(NODE.eq.1) then
          FAL = ALPHA
        end if
C
      end if
C     !END
      call BYE ('TOTS')
C
      return
      end
