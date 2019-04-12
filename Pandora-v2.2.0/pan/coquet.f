      subroutine COQUET
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 25
C---- Prints input.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NO, NPROG
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external LOGIN, DABBLE, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /3/
C
      call HI ('COQUET')
C     !BEG
      call LOGIN  (NPROG)
      call DABBLE (X,IX,W,IW,NO)
      call LOGOUT (NPROG)
C     !END
      call BYE ('COQUET')
C
      return
      end
