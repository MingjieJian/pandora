      subroutine LEPUS
     $(N,XION,ZION)
C
C     Rudolf Loeser, 1989 Sep 14
C---- Computes ZION for diffusion calculations.
C     (This is version 2 of LEPUS.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SMIN, TENTH, X, XION, Z, ZERO, ZION, ZXMIN
      integer I, N
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
      equivalence (RZQ(112),ZXMIN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(19),TENTH )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, HI, BYE
C
C               XION(N), ZION(N)
      dimension XION(*), ZION(*)
C     !EJECT
C
      call HI ('LEPUS')
C     !BEG
      call ZERO1 (ZION,N)
C
      SMIN = TENTH*ZXMIN
C
      do 100 I = 1,N
        X = XION(I)
        if(X.gt.ZERO) then
          if(X.ge.ZXMIN) then
            Z = ONE
          else if(X.le.SMIN) then
            Z = ZERO
          else
            Z = (X-SMIN)/(ZXMIN-SMIN)
          end if
        else
          Z = ZERO
        end if
C
        ZION(I) = Z
C
        if(Z.eq.ZERO) then
          goto 101
        end if
  100 continue
C
  101 continue
C     !END
      call BYE ('LEPUS')
C
      return
      end
