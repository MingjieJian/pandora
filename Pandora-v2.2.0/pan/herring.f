      subroutine HERRING
     $(N,TE,U)
C
C     Rudolf Loeser, 2003 Jan 03
C---- Computes U, for Lyman calculations.
C     (This is version 2 of Herring.)
C     !DASH
      save
C     !DASH
      real*8 CON, CON4, TE, U, XNK, XNUK, ZERO, dummy1, dummy2
      integer N
      logical KILROY
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
      equivalence (RZQ(  9),XNUK )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, HYDATA, ARRDIV, SET1, HI, BYE
C
C               TE(N), U(N)
      dimension TE(*), U(*)
C
      data KILROY /.true./
C
      call HI ('HERRING')
C     !BEG
      if(KILROY) then
        call RIGEL    (4, CON4)
        if(XNUK.eq.ZERO) then
          call HYDATA (0, XNK, dummy1, dummy2)
          CON = CON4*XNK
        else
          CON = CON4*XNUK
        end if
      end if
C
      call SET1       (U, N, CON)
      call ARRDIV     (U, TE, U, N)
C     !END
      call BYE ('HERRING')
C
      return
      end
