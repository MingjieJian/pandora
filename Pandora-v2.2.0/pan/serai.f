      subroutine SERAI
     $(X,IX,TE,XNE,XNC,XNU,XNUC,CKI,CHKI,NO)
C
C     Rudolf Loeser, 2006 Apr 18
C---- Analyzes collisional ionization.
C     (This is version 2 of SERAI.)
C     !DASH
      save
C     !DASH
      real*8 CHKI, CKI, TE, X, XNC, XNE, XNU, XNUC, ZERO
      integer INCEI, IX, J, LIN, N, NO, NSL
C     !COM  or  !DASH
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(225),INCEI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ABJECT, LINER, SHIM, HUND, HI, BYE
C
      dimension X(*), IX(*)
C
C               CHKI(N,NSL), CKI(N,NSL), XNUC(NSL), XNU(NSL), XNC(N),
      dimension CHKI(N,*),   CKI(N,*),   XNUC(*),   XNU(*),   XNC(*),
C
C               XNE(N), TE(N)
     $          XNE(*), TE(*)
C
      call HI ('SERAI')
C     !BEG
      if(NO.gt.0) then
        call ABJECT   (NO)
        write (NO,100) INCEI
  100   format(' ','Collisional ionization at depth #',I4,
     $             ' (when Hydrogen contributes)'//
     $         ' ',12X,'ionization coefficient for',11X,
     $             'ionization rate due to'/
     $         ' ','level',2(10X,'electrons',6X,'Hydrogen'))
        call LINER    (1, NO)
C
        LIN = 0
        do 101 J = 1,NSL
          if(CHKI(INCEI,J).ne.ZERO) then
            call HUND (X, IX, J, TE(INCEI), XNE(INCEI), XNC(INCEI),
     $                 XNU(J), XNUC(J), CKI(INCEI,J), CHKI(INCEI,J),
     $                 NO)
            LIN = LIN+1
            call SHIM (LIN, 5, NO)
          end if
  101   continue
      end if
C     !END
      call BYE ('SERAI')
C
      return
      end
