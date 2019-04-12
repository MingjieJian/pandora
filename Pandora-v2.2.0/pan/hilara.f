      subroutine HILARA
     $(XINT,NRAD,NLAM,FINT,XMIN,XMAX,KOUNT)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Scales an intensity array, for BUZES.
C     !DASH
      save
C     !DASH
      real*8 FINT, HUNDRED, ONE, P, RAT, X, XINT, XMAX, XMIN, ZERO
      integer I, J, KOUNT, NLAM, NRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
C     !EJECT
      external DIVIDE, HI, BYE
C
C               XINT(NRAD,NLAM), FINT(NRAD,NLAM)
      dimension XINT(NRAD,*),    FINT(NRAD,*)
C
      data HUNDRED /1.D2/
C
      call HI ('HILARA')
C     !BEG
      XMIN  = +ZZLARGE
      XMAX  = -ZZLARGE
      KOUNT = 0
C
      do 101 J = 1,NLAM
        do 100 I = 1,NRAD
          X = XINT(I,J)
          if(X.gt.ZERO) then
            KOUNT = KOUNT+1
            if(X.gt.XMAX) then
              XMAX = X
            end if
            if(X.lt.XMIN) then
              XMIN = X
            end if
          end if
  100   continue
  101 continue
C
      if(KOUNT.gt.1) then
        call DIVIDE     (XMAX, XMIN, RAT)
        P = log(RAT)
        call DIVIDE     (HUNDRED, P, P)
        do 103 J = 1,NLAM
          do 102 I = 1,NRAD
            X = XINT(I,J)
            if(X.gt.ZERO) then
              call DIVIDE (X, XMIN, RAT)
              X = log(RAT)
              FINT(I,J) = P*X
            else
              FINT(I,J) = -ONE
            end if
  102     continue
  103   continue
      end if
C     !END
      call BYE ('HILARA')
C
      return
      end
