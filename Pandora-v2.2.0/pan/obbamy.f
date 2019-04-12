      subroutine OBBAMY
     $(X,F,N,TYPE,LABEL,KRET)
C
C     Rudolf Loeser, 1998 Oct 16
C---- Prints an error message, for SMOOTH.
C     !DASH
      save
C     !DASH
      real*8 F, X
      integer KRET, LUEO, N
      character LABEL*(*), TYPE*3
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, LINER, VECOUT, HI, BYE
C
C               X(N), F(N)
      dimension X(*), F(*)
C
      call HI ('OBBAMY')
C     !BEG
      call MESHED ('SMOOTH', 3)
      write (LUEO,100) TYPE,N,KRET,LABEL
  100 format(' ','Error in smoothing routine.',20X,'TYPE = ',A3,5X,
     $           'N =',I5,5X,'KRET =',I3/
     $       ' ',A)
      call VECOUT (LUEO, X, N, 'X')
      call VECOUT (LUEO, F, N, 'F')
      call LINER  (1, LUEO)
      write (LUEO,101)
  101 format(' ','Smoothing has been skipped; unsmoothed values will ',
     $           'be used.')
      call MASHED ('SMOOTH')
C     !END
      call BYE ('OBBAMY')
C
      return
      end
