      subroutine HEATHER
     $(EP1,EP2,BDI,N)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Alternative procedure for EP1 .lt. 0.
C     (This is version 2 of HEATHER.)
C     !DASH
      save
C     !DASH
      real*8 BDI, EP1, EP2, ZERO
      integer IMAX, IMIN, LUEO, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MINMAXD, MESHED, VECOUT, CALLUNA, MASHED, HI, BYE
C
C               EP1(N), EP2(N), BDI(N,NL)
      dimension EP1(*), EP2(*), BDI(*)
C
      call HI ('HEATHER')
C     !BEG
      call MINMAXD   (EP1, 1, N, IMIN, IMAX)
      if(EP1(IMIN).lt.ZERO) then
C
        call MESHED  ('HEATHER', 3)
        write (LUEO,100)
  100   format(' ','Alternative EP1-editing')
        call VECOUT  (LUEO, EP1, N, 'Old EP1')
        call VECOUT  (LUEO, EP2, N, 'Old EP2')
C
        call CALLUNA (EP1, EP2, BDI, EP1(IMIN), N)
C
        call VECOUT  (LUEO, EP1, N, 'New EP1')
        call VECOUT  (LUEO, EP2, N, 'New EP2')
        call MASHED  ('HEATHER')
C
      end if
C     !END
      call BYE ('HEATHER')
C
      return
      end
