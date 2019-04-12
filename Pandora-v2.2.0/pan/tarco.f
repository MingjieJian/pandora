      subroutine TARCO
     $(FO,FN,FRAT,N,KODE,LABEL)
C
C     Rudolf Loeser, 1999 Jan 14
C---- Prints, for TROCAR.
C     !DASH
      save
C     !DASH
      real*8 FN, FO, FRAT
      integer KODE, LUEO, N
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ABJECT, LINER, TAKE, HI, BYE
C
C               FO(N), FN(N), FRAT(N)
      dimension FO(*), FN(*), FRAT(*)
C
      call HI ('TARCO')
C     !BEG
      call ABJECT (LUEO)
      write (LUEO,100) LABEL
  100 format(' ','***** Damping of ',A,10X,'(controlled by KDAMP; ',
     $           'see below).'//
     $       ' ','Ratio = new / old')
      call LINER  (2, LUEO)
      call TAKE   (LUEO, N, FN, 'new', FO, 'old', FRAT)
C
      call LINER  (1, LUEO)
      if(KODE.gt.0) then
        write (LUEO,101)
  101   format(' ','KDAMP = 1: "new" is used in place of "old"')
      else
        write (LUEO,102)
  102   format(' ','KDAMP = 0 ("new" is not used)')
      end if
C     !END
      call BYE ('TARCO')
C
      return
      end
