      subroutine EXPINT
     $(N,X,EI,EX)
C
C     Rudolf Loeser, 1988 Nov 22
C---- Computes EI, the N'th Exponential Integral of X.
C     Also returns EX = exp(-X).
C     (This is version 2 of EXPINT.)
C     !DASH
      save
C     !DASH
      real*8 EI, EX, X, ZERO
      integer M, MTHEI, N
      logical ERROR
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
      equivalence (KZQ(105),MTHEI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external  CEXPINT, PEXPINT, HI, BYE
      intrinsic abs, min
C
      call HI ('EXPINT')
C     !BEG
      if(abs(X).gt.ZLNLARG) then
        EI = ZERO
        EX = ZERO
      else
        if((X.gt.ZERO).and.(MTHEI.eq.1)) then
          call PEXPINT (N,X,EI,EX,ERROR)
        else
          call CEXPINT (N,X,EI,EX)
        end if
      end if
C     !END
      call BYE ('EXPINT')
C
      return
      end
