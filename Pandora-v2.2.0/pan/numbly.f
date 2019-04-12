      subroutine NUMBLY
     $(MN1,ETA,G,RHO,SIGMA,DEL,A,B,C,D,ZETA,CHK)
C
C     Rudolf Loeser, 1997 Aug 25
C---- Prints alternate N1-recalculation intermediates.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DEL, ETA, G, RHO, SIGMA, ZETA
      integer I, LUEO, MN1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MUMBLY, LINER, LING, HI, BYE
C
C               ETA(N), G(N), RHO(N), DEL(N), CHK(N), A(N), B(N), C(N),
      dimension ETA(*), G(*), RHO(*), DEL(*), CHK(*), A(*), B(*), C(*),
C
C               SIGMA(N), D(N), ZETA(N)
     $          SIGMA(*), D(*), ZETA(*)
C
      call HI ('NUMBLY')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Alternate fourdiagonal equations, inward ',
     $           'solution.'///
     $       ' ',15X,'delta',13X,'eta',15X,'g',13X,'rho',11X,'sigma')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,DEL(I),ETA(I),G(I),RHO(I),SIGMA(I),I=1,MN1)
  101 format(5(' ',I4,1P5E16.8/))
C
      call LING   (ZETA, DEL, MN1)
C
      call MUMBLY (MN1, A, B, C, D, CHK)
C     !END
      call BYE ('NUMBLY')
C
      return
      end
