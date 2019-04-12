      subroutine RUMBLY
     $(MN1,ETAB,GB,RHOB,SIGMAB,DELB,A,B,C,D,ZETA,CHK)
C
C     Rudolf Loeser, 1997 Aug 25
C---- Prints alternate N1-recalculation intermediates.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DELB, ETAB, GB, RHOB, SIGMAB, ZETA
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
C               ETAB(N), CHK(N), RHOB(N), DELB(N), ZETA(N), A(N), B(N),
      dimension ETAB(*), CHK(*), RHOB(*), DELB(*), ZETA(*), A(*), B(*),
C
C               SIGMAB(N), C(N), D(N), GB(N)
     $          SIGMAB(*), C(*), D(*), GB(*)
C
      call HI ('RUMBLY')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Alternate fourdiagonal equations, outward ',
     $           'solution.'///
     $       ' ',11X,'delta-bar',9X,'eta-bar',11X,'g-bar',9X,
     $           'rho-bar',7X,'sigma-bar')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,DELB(I),ETAB(I),GB(I),RHOB(I),SIGMAB(I),
     $                  I=1,MN1)
  101 format(5(' ',I4,1P5E16.8/))
C
      call LING   (ZETA, DELB, MN1)
C
      call MUMBLY (MN1, A, B, C, D, CHK)
C     !END
      call BYE ('RUMBLY')
C
      return
      end
