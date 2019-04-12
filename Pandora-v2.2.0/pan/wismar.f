      subroutine WISMAR
     $(P,NP,N,CALLER)
C
C     Rudolf Loeser, 1996 Feb 29
C---- Dumps, for WAGRIN.
C     !DASH
      save
C     !DASH
      real*8 P
      integer I, J, LUEO, N, NP
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MASHED, HI, BYE
C
C               P(N,NP)
      dimension P(N,*)
C
      call HI ('WISMAR')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Ratios to be plotted')
      call LINER  (1, LUEO)
      write (LUEO,101) (I,(P(I,J),J=1,NP),I=1,N)
  101 format(5(' ',I5,1P5E12.5/))
C
      call MASHED (CALLER)
C     !END
      call BYE ('WISMAR')
C
      return
      end
