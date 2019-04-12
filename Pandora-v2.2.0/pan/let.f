      subroutine LET
     $(X,WAVE,ALB)
C
C     Rudolf Loeser, 1984 Feb 02
C---- Drives albedo calculation, for CADMOS.
C     (This is version 3 of LET.)
C     !DASH
      save
C     !DASH
      real*8 ALB, CLM, CQM, PNH, WAVE, X
      integer JJABK, JJALK, JJCQA, JJCQT, JJHND, JJTE, JJXNE, JJZ,
     $        JJZBK, N, NCQ, NKA
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(53),NCQ)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 42),JJALK)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(170),JJZBK)
      equivalence (IZOQ(171),JJABK)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(232),JJCQT)
      equivalence (IZOQ(233),JJCQA)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(101),CQM  )
      equivalence (RZQ(150),PNH  )
      equivalence (RZQ(151),CLM  )
C     !DASH
      external FERE, MOVE1, COMO, HI, BYE
C
      dimension X(*)
C
C               ALB(N)
      dimension ALB(*)
C     !EJECT
C
      call HI ('LET')
C     !BEG
      if(NKA.gt.0) then
        call FERE  (X(JJZBK),1,X(JJABK),1,NKA,X(JJZ),1,X(JJALK),1,N,1,1)
        call MOVE1 (X(JJALK),N,ALB)
      else
        call COMO  (WAVE,X(JJXNE),X(JJTE),X(JJHND),N,X(JJCQT),X(JJCQA),
     $              NCQ,CQM,PNH,CLM,ALB)
      end if
C     !END
      call BYE ('LET')
C
      return
      end
