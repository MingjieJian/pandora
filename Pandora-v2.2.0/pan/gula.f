      subroutine GULA
     $(G,T,RCNL,DELNL,ENN,ELL,ICXDP,J,DMPI)
C
C     Rudolf Loeser, 1990 Nov 29
C---- Computes a value of G(T,n,l) for upper-level charge exchange.
C     !DASH
      save
C     !DASH
      real*8 DELNL, ELL, ENN, F, G, H, RCNL, T, ZERO
      integer ICXDP, J, K, LUEO
      logical DMPI
C     !COM
C---- GAUSH10     as of 1990 Nov 29
      real*8      XGH10,AGH10
      dimension   XGH10(10), AGH10(10)
      common      /GAUSH10/ XGH10,AGH10
C     Roots and weights for 10-point Gauss-Hermite quadrature:
C     Integral(0,inf) [ exp(-x**2) * f{x} dx ] =
C     sum(k=1,10) [ AGH10(k) * f{XGH10(k)} ].
C     .
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
      external LINER, FICUS, HI, BYE
C     !EJECT
C
      call HI ('GULA')
C     !BEG
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO,100) ICXDP,J,ENN,ELL,T,RCNL,DELNL
  100   format(' ','***** Dump of quadrature for G(',I4,',',I3,')',
     $             5X,'n =',F4.0,5X,'l =',F4.0//
     $         ' ',6X,'T =',1PE16.8,5X,'RC(n,l) =',E16.8,5X,
     $             'delta(n,l) =',E16.8)
        call LINER (1, LUEO)
      end if
C
      G = ZERO
      do 102 K = 1,10
        call FICUS (F, XGH10(K), T, RCNL, DELNL, ENN, ELL, H)
        G = G+AGH10(K)*F
C
        if(DMPI) then
          write (LUEO,101) K,XGH10(K),AGH10(K),H,F,G
  101     format(' ',6X,I3,2X,'X =',1PE20.12,2X,'A =',E20.12,
     $               5X,'H =',E16.8,2X,'F =',E16.8,2X,'G =',E16.8)
        end if
C
  102 continue
C     !END
      call BYE ('GULA')
C
      return
      end
