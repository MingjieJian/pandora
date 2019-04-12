      subroutine GOURD
     $(L,OFF,KF,XIF,K,XI,DELTA,IPNT)
C
C     Rudolf Loeser, 1989 Feb 08
C---- Actually makes an XI table for a blended line.
C     (This is version 2 of GOURD.)
C     !DASH
      save
C     !DASH
      real*8 BL, BR, DELTA, HALF, OFF, X, XI, XIF, XL, XR, ZERO
      integer I, IFLG, IPNT, J, K, KF, L
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external COMPD, SORT, HI, BYE
C
C               OFF(LDL), XIF(KF), XI(KM), IPNT(KM)
      dimension OFF(*),   XIF(*),  XI(*),  IPNT(*)
C     !EJECT
C
      call HI ('GOURD')
C     !BEG
C---- Enter first point
      K = 1
      XI(K) = ZERO
C
C---- Enter other points
      do 101 J = 1,L
C----   Get limits for current (j'th) profile fragment
        if(J.eq.1) then
          BL = -ZZLARGE
        else
          BL = OFF(J-1)
        end if
        if(J.lt.L) then
          BR = OFF(J+1)
        else
          BR = +ZZLARGE
        end if
        XL = HALF*(BL    +OFF(J))
        XR = HALF*(OFF(J)+BR    )
C
C----   Enter current profile fragment
        do 100 I = 1,KF
          X = XIF(I)+OFF(J)
          if((XL.le.X).and.(X.le.XR).and.(X.ne.ZERO)) then
            call COMPD (X, XI(K), DELTA, IFLG)
            if(IFLG.ne.0) then
              K = K+1
              XI(K) = X
            end if
          end if
  100   continue
C
  101 continue
C
C---- Sort into increasing order (the zero!)
      call SORT        (XI, K, IPNT, 'Assembled XI')
C     !END
      call BYE ('GOURD')
C
      return
      end
