      subroutine DORE
     $(X,IX)
C
C     Rudolf Loeser, 1980 Aug 25
C---- Reads first part of input.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IX, NPROG
C     !COM
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C     !DASH
      external LOGIN, TANGO, LOGOUT, HI, BYE
C
      dimension X(*), IX(*)
C
      data NPROG /1/
C
      call HI ('DORE')
C     !BEG
      call LOGIN  (NPROG)
      call TANGO  (X,X(KMXWAV+1),IX)
      call LOGOUT (NPROG)
C     !END
      call BYE ('DORE')
C
      return
      end
