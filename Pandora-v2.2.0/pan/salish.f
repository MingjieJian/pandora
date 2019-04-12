      subroutine SALISH
     $(N,KIND,ZP,ZZERO,NO,GOOD)
C
C     Rudolf Loeser, 1999 Aug 04
C---- Jiggles a bad ZZERO result, to force output.
C     !DASH
      save
C     !DASH
      real*8 ZP, ZZERO
      integer KIND, LUEO, N, NO
      logical GOOD
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
C               ZP(N)
      dimension ZP(*)
C
      call HI ('SALISH')
C     !BEG
      KIND  = N
      ZZERO = ZP(N)
C
      NO   = LUEO
      GOOD = .false.
C
      call MESHED ('SALISH', 3)
      write (LUEO,100)
  100 format(' ','Computed TAU5000 is bad.'//
     $       ' ','Ad-hoc values of KIND and ZZERO will be used to ',
     $           'allow the rest of this calculation to proceed;'/
     $       ' ','the run will be stopped after the printout for ',
     $           'this calculation is complete.')
      call MASHED ('SALISH')
C     !END
      call BYE ('SALISH')
C
      return
      end
