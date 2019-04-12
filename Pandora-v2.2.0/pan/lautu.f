      subroutine LAUTU
     $(N,NL,RNDU,RND,KODE,PLK,PKL,SPKL)
C
C     Rudolf Loeser, 1990 May 15
C---- Dumps, for TARPON.
C     !DASH
      save
C     !DASH
      real*8 PKL, PLK, RND, RNDU, SPKL
      integer KODE, LUEO, N, NL
      logical PRNTZ
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, OMAR, VECOUT, HI, BYE
C
C               RND(N,NL), RNDU(N,NL), PLK(N,NL), PKL(N,NL), SPKL(N)
      dimension RND(N,*),  RNDU(N,*),  PLK(N,*),  PKL(N,*),  SPKL(*)
C
      data PRNTZ /.true./
C
      call HI ('LAUTU')
C     !BEG
      call LINER   (2, LUEO)
      write (LUEO,100)
  100 format(' ','RND - relative number densities')
      if(KODE.gt.0) then
        call OMAR  (LUEO, N, NL, RNDU, 'Level ',PRNTZ)
        call LINER (2, LUEO)
        write (LUEO,101) (KODE-1)
  101   format(' ','RND - relative number densities - smoothed ',
     $             'values (',I6,' values were changed)')
      end if
      call OMAR    (LUEO, N, NL, RND, 'Level ',PRNTZ)
C
      call LINER   (2, LUEO)
      write (LUEO,102)
  102 format(' ','(PLK) Pnk - ionization rates from each level')
      call OMAR    (LUEO, N, NL, PLK, 'Level ',PRNTZ)
C
      call LINER   (2, LUEO)
      write (LUEO,103)
  103 format(' ','(PKL) Pkn - recombination rates to each level')
      call OMAR    (LUEO, N, NL, PKL, 'Level ',PRNTZ)
C
      call LINER   (1, LUEO)
      call VECOUT  (LUEO, SPKL, N, 'SPKL - sum (over levels) of PKL')
C     !END
      call BYE ('LAUTU')
C
      return
      end
