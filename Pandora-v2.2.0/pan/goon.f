      subroutine GOON
     $(TAU5000,ZP,ZZERO,N,KIND,LU,GOOD)
C     Rudolf Loeser, 1980 Nov 14
C---- Computes ZZERO, for CAKE.
C     ZZERO is the value of ZP where TAU5000=1 occurs.
C     (This is version 4 of GOON.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, ONE, TAU5000, XDEN, XNUM, ZP, ZZERO
      integer IPEX, KIND, LOOK, LU, LUEO, N, NOTE
      logical GOOD
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external LOOKSD, DIVIDE, SALISH, MESHED, MASHED, HI, BYE
C
C               TAU5000(N), ZP(N)
      dimension TAU5000(*), ZP(*)
C
      data DELTA /1.D-10/
C
      call HI ('GOON')
C     !BEG
C---- Find KIND such that TAU5000(KIND) .le. ONE .lt. TAU5000(KIND+1)
      call LOOKSD   (TAU5000, N, DELTA, ONE, KIND, NOTE, LOOK)
C
      if(LOOK.ne.1) then
C----   Error; jiggle things for printout purposes
        call SALISH (N, KIND, ZP, ZZERO, LU, GOOD)
      else
C
C----   Compute ZZERO
        XNUM = ZP(KIND  )*(TAU5000(KIND+1)-ONE)
     $        +ZP(KIND+1)*(ONE-TAU5000(KIND  ))
        XDEN = TAU5000(KIND+1)-TAU5000(KIND)
        call DIVIDE (XNUM, XDEN, ZZERO)
      end if
C----
      if((IPEX.lt.0).or.(IPEX.eq.6)) then
        call MESHED ('GOON', 2)
        write (LUEO,100) KIND,N,TAU5000(KIND),ZP(KIND),
     $                   TAU5000(KIND+1),ZP(KIND+1),ZZERO
  100   format(' ',2I5,1P5E20.12)
        call MASHED ('GOON')
      end if
C     !END
      call BYE ('GOON')
C
      return
      end
