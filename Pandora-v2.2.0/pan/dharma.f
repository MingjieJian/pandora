      subroutine DHARMA
     $(X,IX,W,IW,IMG)
C
C     Rudolf Loeser, 1976 Oct 27
C---- Computes default B-ratios.
C     (This is version 2 of DHARMA.)
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer IMG, IW, IX, JJBIJ, NT
      character LEGEND*33
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 23),JJBIJ)
C     !DASH
      external FLIP, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               IMG(N)
      dimension IMG(*)
C
      data LEGEND /'Default b-ratios (no transitions)'/
C
      call HI ('DHARMA')
C     !BEG
      if(NT.le.0) then
        call FLIP (X,IX,W,IW,1,dummy,dummy,dummy,X(JJBIJ),0,0,LEGEND,
     $             dummy,IMG)
      end if
C     !END
      call BYE ('DHARMA')
C
      return
      end
