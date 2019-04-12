      subroutine PIT
     $(N,EPCBR,SPKL,PS1,DUMP)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Prints details, for ALMOND.
C     !DASH
      save
C     !DASH
      real*8 EPCBR, PS1, SPKL
      integer LUEO, N
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               SPKL(N), PS1(N)
      dimension SPKL(*), PS1(*)
C
      call HI ('PIT')
C     !BEG
      if(DUMP) then
        call LINER  (2, LUEO)
        write (LUEO,100) EPCBR
  100   format(' ','EPCBR =',1PE16.8)
C
        call VECOUT (LUEO, SPKL, N, 'SPKL')
        call VECOUT (LUEO, PS1 , N, 'PS1')
      end if
C     !END
      call BYE ('PIT')
C
      return
      end
