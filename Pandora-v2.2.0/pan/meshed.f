      subroutine MESHED
     $(CALLER,N)
C
C     Rudolf Loeser, 2002 Oct 11
C---- Prints special output headers.
C     !DASH
      save
C     !DASH
      integer LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, MOSHED, HI, BYE
C
      call HI ('MESHED')
C     !BEG
      if(N.eq.2) then
        write (LUEO,100) CALLER
  100   format(' ','$&%:',5X,A,15X,'Special Output')
      else if(N.eq.1) then
        call LINER (1, LUEO)
        write (LUEO,101) CALLER
  101   format(' ','$%&:',5X,A,15X,'Stop Request')
      else if(N.eq.3) then
        call LINER (1, LUEO)
        write (LUEO,102) CALLER
  102   format(' ','$%&:',5X,A,15X,'Message')
      else if(N.eq.4) then
        call LINER (1, LUEO)
        write (LUEO,103) CALLER
  103   format(' ','$%&:',5X,A,15X,'Special Output and/or Messages')
      end if
C
      call DASHER  (LUEO)
      call LINER   (1, LUEO)
C
      call MOSHED  (CALLER, 2)
C     !END
      call BYE ('MESHED')
C
      return
      end
