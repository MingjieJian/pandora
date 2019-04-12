      subroutine OLDMOR
     $(NO,NAME)
C
C     Rudolf Loeser, 2006 Apr 21
C---- Prints an explanation for the ATOM printout.
C     !DASH
      save
C     !DASH
      integer NO
      character NAME*2
C     !DASH
      external LINER, HI, BYE
C
      call HI ('OLDMOR')
C     !BEG
      call LINER (1, NO)
      write (NO,100) NAME,NAME,NAME
  100 format(' ',8X,'* Values of ',A2,'(T) not specified in the ',
     $           'input are computed by the program.'/
     $       ' ',10X,'Several methods are available for this purpose, '
     $           'as chosen by the user with the ',A2,'METHOD input ',
     $           'statement.'/
     $       ' ',10X,'Some or all of the ',A2,'-values of this run ',
     $           'were computed using:')
C     !END
      call BYE ('OLDMOR')
C
      return
      end
