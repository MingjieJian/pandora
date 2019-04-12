      subroutine HOUND
     $(NO,IMAGE,LFB)
C
C     Rudolf Loeser, 1980 Nov 14
C---- Prints Spectrum Summary graph, for LEDGER.
C     !DASH
      save
C     !DASH
      integer LFB, N, NO
      character FACELAB*10, IMAGE*(*), LINE*117
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external  ABJECT, LINER, HADDOCK, KPRINT, KGIVE, TUMBLE, HI, BYE
C
      call HI ('HOUND')
C     !BEG
C---- Print finished graph
      call TUMBLE  (LFB,FACELAB)
      call ABJECT  (NO)
      write (NO,100) FACELAB
  100 format(' ','Graph of TE and TB vs. depth index',87X,A10)
      call LINER   (1,NO)
C
      call KPRINT  (IMAGE,NO)
C---- Make and print a set of abscissa labels
      call HADDOCK (N,IMAGE)
C
      call KGIVE   (IMAGE,1,LINE)
      write (NO,101) LINE
      call KGIVE   (IMAGE,2,LINE)
      write (NO,101) LINE
  101 format(' ',10X,A117)
C     !END
      call BYE ('HOUND')
C
      return
      end
