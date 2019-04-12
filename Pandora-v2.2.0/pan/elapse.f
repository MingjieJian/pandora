      subroutine ELAPSE
     $(NTITER,T5,T5A)
C
C     Rudolf Loeser, 2002 Jan 22
C---- Computes adjustment factor, for PLEASE.
C     !DASH
      save
C     !DASH
      real*8 HTAU, ONE, T5, T5A, T5U
      integer NTITER
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
      equivalence (RZQ( 71),HTAU )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               T5(12)
      dimension T5(*)
C
      call HI ('ELAPSE')
C     !BEG
      if(NTITER.gt.1) then
        T5U = HTAU*T5(NTITER)+(ONE-HTAU)*T5(NTITER-1)
      else
        T5U = T5(1)
      end if
      T5A = sqrt(T5U)
C     !END
      call BYE ('ELAPSE')
C
      return
      end
