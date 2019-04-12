      subroutine MASHED
     $(CALLER)
C
C     Rudolf Loeser, 2003 Oct 08
C---- Prints special output trailers.
C     !DASH
      save
C     !DASH
      integer J, LUEO
      character CALLER*(*), FILLER*48, MSHCLR*40
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  LINER, MOSHED, HI, BYE
      intrinsic len
C
      data FILLER /'................................................'/
C
      call HI ('MASHED')
C     !BEG
      J = len(CALLER)
      call MOSHED   (MSHCLR, 1)
      if(MSHCLR(:J).eq.CALLER) then
C
        call LINER  (1, LUEO)
        write (LUEO,100) CALLER,FILLER
  100   format(' ','%&$:',5X,A,15X,'End of Output',T81,A)
C
      else
        write (*,101) CALLER,MSHCLR(:J)
  101   format(' ','MASHED: ',A,' (caller) differs from ',A,
     $             ' (mshclr).')
      end if
C     !END
      call BYE ('MASHED')
C
      return
      end
