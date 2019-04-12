      subroutine EVE
     $(NAME,KOUNT,LIMIT,KILROY,CALLER)
C
C     Rudolf Loeser, 1981 May 26
C---- Tests "KOUNT" and writes message, for ADAM.
C     (This is version 2 of EVE.)
C     !DASH
      save
C     !DASH
      integer KOUNT, LIMIT, LUEO
      logical KILROY
      character CALLER*(*), NAME*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUSHED, HI, BYE
C
      call HI ('EVE')
C     !BEG
      if(KOUNT.gt.LIMIT) then
        call MUSHED (CALLER, 1, KILROY)
        write (LUEO,100) NAME,KOUNT,LIMIT
  100   format(' ',A10,' = ',I3,' is bigger than ',I3,'.')
      end if
C     !END
      call BYE ('EVE')
C
      return
      end
