      subroutine DRUMLIN
     $(N,Z,A,XNE,XNP,XNH1,XNHE11,XNHEP,XNHE2K)
C
C     Rudolf Loeser, 1984 May 08
C---- Dumps, for LOCKET.
C     !DASH
      save
C     !DASH
      real*8 A, XNE, XNH1, XNHE11, XNHE2K, XNHEP, XNP, Z
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               Z(N), XNP(N), XNH1(N), XNHE11(N), XNHEP(N), XNHE2K(N),
      dimension Z(*), XNP(*), XNH1(*), XNHE11(*), XNHEP(*), XNHE2K(*),
C
C               A(N), XNE(N)
     $          A(*), XNE(*)
C
      call HI ('DRUMLIN')
C     !BEG
      write (LUEO,100)
  100 format(' ','Input for particle energy dissipation calculation'//
     $       ' ',17X,'Z',14X,'A',13X,'NE',13X,'NP',12X,'NH1',10X,
     $           'NHE11',11X,'NHEP',10X,'NHE2K')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,Z(I),A(I),XNE(I),XNP(I),XNH1(I),XNHE11(I),
     $                 XNHEP(I),XNHE2K(I),I=1,N)
  101 format(5(' ',I3,1P8E15.7/))
C
      call LINER (5, LUEO)
      write (LUEO,102)
  102 format(' ','Details of integration steps for particle energy ',
     $           'dissipation calculation')
C     !END
      call BYE ('DRUMLIN')
C
      return
      end
