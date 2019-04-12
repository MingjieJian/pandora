      subroutine FLASK
     $(X,W,IW)
C
C     Rudolf Loeser, 2002 Apr 18
C---- Recomputes temperature gradients.
C     (This is version 2 of FLASK.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IVEC, IW, JJDTE, JJTE, JJZ, JJZT, MOX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(178),JJZT )
      equivalence (IZOQ(234),JJDTE)
C     !DASH
      external LOT, WAGON, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IVEC  )
C
      call HI ('FLASK')
C     !BEG
C     (Get W allotment)
      call LOT   (IN,IS,MOX,'FLASK')
C
      call WAGON (N,X(JJTE),X(JJZ),X(JJZT),X(JJDTE),W(IVEC),W,IW)
C
C     (Give back W allotment)
      call WGIVE (W,'FLASK')
C     !END
      call BYE ('FLASK')
C
      return
      end
